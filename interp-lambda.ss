(define first car)
(define second cadr)
(define third caddr)

;;;;;functions to deal with closure
(define closure
  (lambda (exp env)
    (list env exp)))     ;;considered the data-of function
(define table-of car)
(define func-of cadr)

(define interp0
  (lambda (exp env)
    (cond
     ((list? exp)
      (let((x (car exp)))
	(cond
	 ((list? x)(*application exp env))
	 ((eq? 'lambda x) (closure exp env))
         ((eq? x 'quote)exp)
	 (else (atom-to-action x exp env)))))
     (else (atom-to-value exp env)))))
(define interp
  (lambda (exp)
    (data-of (interp0 exp env0))))
(define atom-to-action
  (lambda(x exp env)
    (cond
     ((eq? x '+)(+ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '-)(- (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '*)(* (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '/)(/ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x 'number?)(number?(interp0 (second exp) env)))
     ((eq? x 'eq?)(eq? (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x 'cond)(ev-cond (cdr exp)env))
     (else (list-operate x exp env)))))


(define atom-to-value
  (lambda (x env)
    (cond
     ((number? x)x)
     (else(look-up x env)))))


;;;;;;;;;;;;;;;;;;;;;functions deal with quote expressions;;;;;;;;;;;;;;;;;;;;;;


(define add-type
  (lambda (ls)
    (list 'quote ls)))
(define interp1
  (lambda(exp env)
    (data-of (interp0 exp env))))
(define data-of
  (lambda(x)
    (cond
      ((list? x)(cadr x))
      (else x))))

(define is-list?
  (lambda(x)
    (eq? (car x) 'quote)))
(define list-operate
  (lambda(x exp env)
    (cond
     ((eq? x 'car)(car(interp1(second exp)env)))
     ((eq? x 'cdr)(cdr(interp1(second exp)env)))
     ((eq? x 'cons)(add-type(cons(interp1 (second exp) env)
				 (interp1(third exp)env))))
     ((eq? x 'pair?)(pair? (interp1 (second exp) env)))
     ((eq? x 'list?)(list?(interp1(second exp) env)))
     ((eq? x 'symbol?)(symbol?(interp1 (second exp)env)))
     (else (symbol-to-action x exp env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;functions deal with applications;;;;;;;;;;;;;;;
(define symbol-to-action
  (lambda (x exp env)
    (let ((y (look-up x env)))
      (cond
       ((list? y)
        (cond
	 ((is-list? y)y)
	 (else (interp0 
		(list (func-of y) (eva-para exp env))
		(table-of y)))))
       (else y)))))
(define *application
  (lambda(exp env)
    (cond
     ((eq? (caar exp) 'lambda)
      (interp0 (body-of exp) 
	       (extend-env (formals-of exp) (eva-para exp env0) env)))
     (else (let ((v1 (interp0 (car exp)env))
                 (v2 (second exp)))
             (interp0 (list (func-of v1) (interp v2)) (table-of v1)))))))

(define body-of caddar)
(define formals-of caadar)
(define eva-para
  (lambda(exp env)
    (interp0 (cadr exp) env)))


;;;;;;;;;;;;;;;;;functions deal with symbol table;;;;;;;;;;;;;;;
(define env0 '(()()))
(define extend-env
  (lambda (var val env)
    (list (cons var (first env))(cons val (second env)))))
(define look-up
  (lambda (var env)
    (define look-up-help
      (lambda (name var-ls val-ls)
	(cond
	 ((null? var-ls)(value-error x))
	 ((eq? var (car var-ls))
	  (car val-ls))
	 (else(look-up-help var (cdr var-ls)(cdr val-ls))))))
    (look-up-help var (first env)(second env))))

;;= = == = = = = = = = == test= = = = = = = = = = = = = = = =
;;(interp '((lambda(x)(cons x '()))'x))
;;(interp '((lambda(x)(x 1))(lambda(x)(+ 100 x))))
;;(interp '((lambda (x)
;;            (cond
;;              ((eq? x 1)1)
;;              (else 0)))1))
;;(interp '((((lambda(x)(lambda(y)(lambda(x) x)))1)2)3))
