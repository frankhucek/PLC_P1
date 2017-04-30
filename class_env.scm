; class environment
(load "environment.scm")

(define testenv1 (box '((((main a b) ((() ((var a (new A)) (return (+ (dot a x) (dot a y))))) 15 9))))  ))
(define testenv2 (box '((((main y x) ((() ((var a (new A)) (return (+ (dot a x) (dot a y))))) 10 5))))  ))
(define test-class-env (cons '(class1 class2)
                             (cons (cons (cons '() (cons testenv2 '()))
                                         (cons (cons '() (cons testenv1 '())) '())
                                         ) '())) )

#|
Create helper functions to create a new class and instance and to access the portions of a class and instance.
The class must store the parent class, the list of instance fields, the list of methods/closures,
and (optionally) a list of class fields/values and a list of constructors.
Use your state/environment structure for each of these lists.
The instance must store the instance's class (i.e. the run-time type or the true type) and a list of instance field values.
      (classname1 (a b c)) where (a b c) is the list of instance field values
|#

(define new-class-env (lambda () (cons '() (cons '() '() ))))
(define class-names (lambda (class_env) (car class_env)))
(define class-bodies (lambda (class_env) (car (cdr class_env)) ) )
(define first-class-name (lambda (env) (caar env)))
(define first-class-info (lambda (env) (car (class-bodies env))))
(define first-class-body (lambda (env) (cadr (first-class-info env))))
(define the-working-env (lambda (env) (car env)))

(define addWorkingEnv
  (lambda (class_env)
    (cons (newenv) (cons class_env '()))))

(define lookup-in-class-with-working-env
  (lambda (var class-name env)
    (lookup-in-class var class-name (cadr env))))
     
(define lookup-in-class
  (lambda (var class-name env)
    (lookup var (class-body-of class-name env))))

;this else probs wont work with mutiple classes need fixing
(define class-body-of
  (lambda (class-name env)
    (cond
      ((eq? class-name (first-class-name env)) (first-class-body env))
      (else (class-body-of class-name
                           (cons (cdr (class-names env)) (cdr (class-bodies env))))))))

(define insert-class
  (lambda (class-name class-parent class-body class-env)
    (cons (cons class-name (class-names class-env))
          (cons (cons (cons class-parent (cons class-body '())) (class-bodies class-env)) '())) ))

(define class-instance-fields
  (lambda (class-name class-env)
    (instance-fields (class-body-of class-name class-env))))
