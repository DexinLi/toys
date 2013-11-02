(define first car)
(define second cadr)
(define third caddr)

(define atom-to-action
  (lambda(x exp env)
    (cond
     ((eq? x 'car)(car (interp0(second exp)env)))
     ((eq? x 'cdr)(cdr (interp0(second exp)env)))
     ((eq? x '+)(+ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '-)(- (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '*)(* (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x '/)(/ (interp0(second exp)env) (interp0(third exp)env)))
     ((eq? x 'quote)(second exp))
     ((eq? x 'cons)(cons (interp0(second exp) env) (interp0(third exp)env)))
     ((eq? x 'list?)(list? (interp0(second exp) env)))
     ((eq? x 'number?)(number? (interp0(second exp)env)))
     ((eq? x 'symbol?)(symbol? (interp0(second exp)))))))


(define body-of caddar)
(define formals-of caadar)
(define form-of cdr)
(define table-of car)

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

(define extend-env
  (lambda (var val env)
    (list (cons var (first env))(cons val (second env)))))


(define atom-to-value
  (lambda (exp env)
    (cond
      ((number? exp)exp)
      ((eq? exp #t)#t)
      ((eq? exp #f)#f)
      (else (look-up exp env)))))

(define *application
  (lambda(exp env)
    (cond
     ((eq? (caar exp) 'lambda)
      (interp0 (body-of exp) (extend-env (formals-of exp) (interp(cadr exp))env)))
     (else (let ((v1 (interp0 (car exp)env))
                 (v2 (second exp)))
             (interp0 (list (form-of v1) (interp v2)) (table-of v1)))))))


(define closure cons)
(define env0 '(()()))

(define interp0
  (lambda (exp env)
    (cond
      ((list? exp)
       (let((x (car exp)))
         (cond
           ((list? x)(*application exp env))
           ((eq? 'lambda x) (closure env exp))
	   (else (atom-to-action x exp env)))))
      (else(atom-to-value exp env)))))


(define interp
  (lambda (exp)
    (interp0 exp env0)))



;;==============test======================
;;(interp '((lambda(x)(cons x '()))'x))
(interp '((((lambda(x)(lambda(y)(lambda(x) x)))1)2)3))