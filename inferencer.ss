;; a type inferencer for simply typed lambda calculus


(define name-of car)
(define type-of cdr)
(define build cons)
(define bind list)
(define var? vector?)
(define var vector)

(define fatal
  (lambda (who . args)
    (display who) (display ": ")
    (for-each display args)
    (display "\n")
    (error 'infer "")))


(define map*
  (lambda (func ls)
    (cond
      ((pair? ls)
       (cond
         ((null? (cdr ls))
          (func (car ls)))
         (else (cons (func (car ls)) (map* func (cdr ls))))))
      (else (func ls)))))

(define look-up             ;;return a list present the type
  (lambda(v env)
    (cond
      ((pair? v)(cons (look-up (car v) env) (look-up (cdr v) env)))
      (else (let ((x(assq v env)))
              (cond
                ((not x) v)
                ((pair? (type-of x))
                 (map* (lambda (x)(look-up x env)) (type-of x)))
                ((var? (type-of x))(look-up (type-of x) env))
                (else (type-of x))))))))

(define unify
  (lambda (v w env)
    (let ((t1 (look-up v env))
          (t2 (look-up w env)))
      (cond
        ((eq? t1 t2) env)
        ((var? t1)
         (if (occurs t1 t2)
             #f
             (cons `(,t1 . ,t2) env)))
        ((var? t2)
         (if (occurs t2 t1)
             #f
             (cons `(,t2 . ,t1) env)))
	((and (pair? t1) (pair? t2)
	 (let ((s (unify (car t1) (car t2) env)))
	   (and s (unify (cdr t1) (cdr t2) s)))))
	((equal? t1 t2) env)
        (else #f)))))

(define occurs
  (lambda (t1 t2)
    (cond
      ((pair? t2)(or (eq? t1 (car t2)) (occurs t1 (cdr t2))))
      (else (eq? t1 t2)))))

(define env0 '((+ int int . int)
	       (- int int . int)
	       (* int int . int)
	       (<  int int . bool)
	       (>  int int . bool)
	       (=  int int . bool)
	       (>= int int . bool)
	       (<= int int . bool)
	       (and bool bool . bool)
	       (or bool bool . bool)
               (zero? int . bool)
               (add1 int . int)
               (sub1 int . int)))


(define first car)
(define second cadr)

(define infer
  (lambda (exp)
    (print-type
     (infer0 (alpha-conv exp)))))


(define alpha-conv
  (lambda (exp)
    (let ((n -1)(env '()))
      (letrec ((v-name
		(lambda ()
		  (set! n (+ 1 n))
		  (var n)))
	       (ext (lambda (x v)
		      (set! env (cons `(,x . ,v) env))))
	       (alpha (lambda (exp)
			(cond
			 ((null? exp)'())
			 ((list? exp)
			  (let ((x (car exp)))
			    (cond
			     ((eq? 'lambda x)
                              (let ((v (v-name)))
				(ext (caadr exp) v)
				(append `(,x (,v)) (alpha (cddr exp)))))
			     (else (map alpha exp)))))
			 (else
			  (let ((v (assq exp env)))
			    (cond
			     ((not v) exp)
			     (else (cdr v)))))))))
	(cons (alpha exp) env)))))

(define print-type
  (lambda (ls)
    (let ((n -1) (env '()))
      (letrec ((t-name (lambda ()
			 (set! n (+ 1 n))
			 (string->symbol
			  (string-append "t" (number->string n)))))
               (set-env!(lambda (ls w)
			  (set! env (cons `(,ls . ,w) env))
			  w))
	       (print (lambda (ls)
			(cond
			 ((pair? ls)
			  (cond 
			   ((null? (cdr ls))(print (car ls)))
			   (else (list (print (car ls))
				       '->
				       (print (cdr ls))))))
			 ((var? ls)
			  (let ((v (assq ls env)))
			    (cond
			     ((not v) (set-env! ls (t-name)))
			     (else (cdr v)))))
			 (else ls)))))
	(print ls)))))

(define reify
  (lambda (exp s0)
    (define find
      (lambda (x s0)
        (cond
          ((null? s0)#f)
          ((eq? (cdar s0) x) (caar s0))
          (else (find x (cdr s0))))))
    (cond
     ((list? exp) (map (lambda (x) (reify x s0)) exp))
     (else
      (let ((v (find exp s0)))
	(cond
	 ((not v) exp)
	 (else v)))))))

(define infer0
  (lambda (exp-s0)
    (let ((exp (car exp-s0))
	  (s0 (cdr exp-s0)))
      (let ((n 0)(env env0))
	(letrec 
	    ((fresh-name (lambda () 
			   (set! n (+ 1 n))
			   (var 't n)))
	     (atom-to-type (lambda (exp)
			     (cond
			      ((number? exp)'int)
			      ((boolean? exp)'bool)
                              ((string? exp) 'string)
                              (else (look-up exp env)))))
	     (closure (lambda (exp)
			(cons (caadr exp) (infer1 (caddr exp)))))
             (app-aux (lambda (t1 t2)
                        (let ((s (unify (car t1) t2 env)))
                          (cond
                            ((not s)
                             (fatal 'infer
                                    "wrong argument type              \n"
                                    " - function:      " (reify exp s0)  "\n"
                                    " - function type: " (print-type (look-up t1 env)) "\n"
                                    " - argument type: " (print-type (look-up t2 env)) "\n"))
                            (else (set! env s)
                                  (cdr t1))))))
	     (application (lambda (exp)
                            (let ((args (infer1 (cadr exp)))
                                  (func (infer1 (car exp)))
                                  (re-t (fresh-name)))
                              (cond
                                ((var? func)
                                 (let ((t (fresh-name)))
                                   (set! env (cons `(,func ,t . ,re-t) env))
                                   (app-aux (look-up func env) args)))
                                (else (app-aux func args))))))
	     (in-if (lambda (exp)
			  (let ((test (infer1 (cadr exp)))
				(t-branch (infer1 (caddr exp)))
				(f-branch (infer1 (cadddr exp))))
			    (let ((t (unify test 'bool env)))
				  (cond
				   ((not t)
				    (fatal 'infer "test is not of type bool    \n" 
					   "expression:   " (reify exp s0)  "\n"
					   "type:         " (print-type (look-up test env))))
				   (else (set! env t)
					 (let ((branch (unify t-branch f-branch t)))
					   (cond
					    ((not branch)
					     (fatal 'infer "branches must have the same type       \n"
						    " - expression:        " (reify exp s0)  "\n"
						    " - true branch type:  " (print-type (look-up t-branch env))   "\n"
						    " - false branch type: " (print-type (look-up f-branch env))))
					    (else (set! env branch) t-branch)))))))))
	     (infer1 (lambda (exp)
		       (cond
			((list? exp)
			 (let ((x (car exp)))
			   (cond
			    ((list? x) (application exp))
			    ((eq? 'lambda x) (closure exp))
			    ((eq? 'if x)(in-if exp))
			    (else (application exp)))))
			(else (atom-to-type exp)))))) 
          (let ((t (infer1 exp)))
            (look-up t env)))))))


(infer 1)
; => int

(infer #t)
; => bool

(infer '(lambda (v) v))
; => (t0 -> t0)

(infer '(lambda (f) (lambda (x) (f x))))
; => ((t0 -> t1) -> (t0 -> t1))

(infer '(lambda (f) (lambda (x) (f (f x)))))
; => ((t0 -> t0) -> (t0 -> t0))

(infer '((lambda (f) (lambda (x) (f (f x)))) add1))
; => (int -> int)

(infer '(if (zero? 1) #t #f))
; => bool

(infer '(lambda (f) (lambda (x) (f ((+ x) 1)))))
; => ((int -> t0) -> (int -> t0))

(infer '(lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m (n f)) x))))))
; => ((t0 -> (t1 -> t2)) -> ((t3 -> t0) -> (t3 -> (t1 -> t2))))

(infer '((lambda (f) (f 1)) (lambda (v) v)))
; => int

(infer '(if (zero? 1) #f #t))
; => bool

(define S '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define K '(lambda (x) (lambda (y) x)))

(infer S)
; => ((t0 -> (t1 -> t2)) -> ((t0 -> t1) -> (t0 -> t2)))

(infer `(,S ,K))
; => ((t0 -> t1) -> (t0 -> t0))

(infer `((,S ,K) ,K))
; => (t0 -> t0)

; incorrect programs

;; infer: trying to apply function to wrong type argument:
;;  - function:      f
;;  - function type: (t0 -> t1)
;;  - expected type: t0
;;  - argument type: (t0 -> t1)
;;  - argument: f

(infer '(if (zero? 1) #t 1))
;; infer: branches of conditional must have the same type
;;  - expression:        (if (zero? 1) #t 1)
;;  - true branch type:  bool
;;  - false branch type: int

(infer '((lambda (x) ((+ 1) x)) "hello"))
;; infer: trying to apply function to wrong type argument:
;;  - function:      (lambda (x) ((+ 1) x))
;;  - function type: (int -> int)
;;  - expected type: int
;;  - argument type: string
;;  - argument: hello
