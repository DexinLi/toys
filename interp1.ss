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
	 (else (atom-to-action x exp env)))))
     (else (atom-to-value exp env)))))

(define interp
  (lambda (exp)
    (data-of (interp0 exp env0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;functions deal with list type;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define atom-to-action
  (lambda(x exp env)
    (cond
     ((eq? x '+)(+ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '-)(- (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '*)(* (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '/)(/ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '=)(= (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '<)(< (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '>)(> (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x 'quote)exp)
     ((eq? x 'number?)(number?(interp0 (second exp) env)))
     ((eq? x 'eq?)(eq? (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x 'cond)(ev-cond (cdr exp)env))
     ((eq? x 'let)(ev-let (cdr exp) env))
     (else (list-operate x exp env)))))
(define is-list?
  (lambda(x)
    (cond
      ((list? x) (eq? (car x) 'quote))
      (else #f))))
(define list-operate
  (lambda(x exp env)
    (cond
     ((eq? x 'car)(car (interp1 (second exp) env)))
     ((eq? x 'cdr)(cdr (interp1 (second exp) env)))
     ((eq? x 'cons)(add-type (cons (interp1 (second exp) env)
				   (interp1 (third exp) env))))
     ((eq? x 'pair?)(pair? (interp1 (second exp) env)))
     ((eq? x 'list?)(list? (interp1 (second exp) env)))
     ((eq? x 'symbol?)(symbol? (interp1 (second exp) env)))
     ((eq? x 'list)(eva-para exp env))
     (else (symbol-to-action x exp env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define atom-to-value
  (lambda (x env)
    (cond
     ((number? x)x)
     ((eq? x #t)#t)
     ((eq? x #f)#f)
     (else(look-up x env)))))

(define symbol-to-action
  (lambda (x exp env)
    (let ((y (look-up x env)))
      (cond
       ((list? y)
        (cond
	 ((is-list? y)(type-error x y))
	 (else (*application 
		(cons (func-of y) (eva-para exp env))
		(table-of y)))))
       (else (type-error x y))))))


(define *application
  (lambda(exp env)
    (cond
     ((eq? (caar exp) 'lambda)
      (interp0 (body-of exp) 
	       (extend-env (formals-of exp) (eva-para exp env) env)))
     (else (let ((v1 (interp0 (car exp) env))
                 (v2 (second exp)))
             (interp0 (list (func-of v1) (eva-para v2 env)) (table-of v1)))))))
(define body-of caddar)
(define formals-of cadar)
(define eva-para
  (lambda(exp env)
    (map
     (lambda(x)
       (interp0 x env0))
     (cdr exp))))



;;;;;;;;;;;;;;;;;;;;;;;;;;deal with the cond and let situation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define branch-of car)
(define question-of car)
(define answer-of cadr)

(define ev-cond
  (lambda (cond-body env)
    (let((x (branch-of cond-body)))
      (cond
       ((eq? (question-of x) 'else) (interp0 (answer-of x)env))
       ((interp0 (question-of x)env) (interp0 (answer-of x)env))
       (else (ev-cond (cdr cond-body) env))))))

(define binders-of car)

(define name-of
  (lambda (x)
    (cons (caar x)'())))
(define val-of cdar)

(define ev-let
  (lambda (let-body env)
    (let ((binders (binders-of let-body)))
      (cond
       ((null? binders)(interp0 (cdr let-body) env))
       (else (ev-let (cons (cdr binders) (cdr let-body)) (extend-env (name-of binders) (val-of binders) env)))))))



;;;;;;;;;;;;;;functions deal with symbol table;;;;;;;;;;;;;;;;;;;;;;;
(define env0 '(()()))
(define extend-env
  (lambda (var val env)
    (list (append var (first env))(append val (second env)))))
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
;;(interp '((lambda(x) (x (quote x) (quote ()))(quote (x)))(lambda (x y) (cons x y))))
;;(interp '(let
;;             ((x 5))
;;          x))
;;(interp '((lambda(x)(x 1))(lambda(x)(+ 100 x))))
;;(interp '((lambda (x)
;;            (cond
;;              ((eq? x 1)1)
;;              (else 0)))0))
;;(interp '((((lambda(x)(lambda(y)(lambda(x) x)))1)2)3))
