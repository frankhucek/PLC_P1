; class environment
(load "environment.scm")

(define testenv1 (box '((((main a b) ((() ((var a (new A)) (return (+ (dot a x) (dot a y))))) 15 9))))  ))
(define testenv2 (box '((((main y x) ((() ((var a (new A)) (return (+ (dot a x) (dot a y))))) 10 5))))  ))
(define test-class-env (cons '(class1 class2)
                             (cons (cons (cons '() (cons testenv2 '()))
                                         (cons (cons '() (cons testenv1 '())) '())
                                         ) '())) )

(define new-class-env (lambda () (cons '() (cons '() '() ))))
(define class-names (lambda (class_env) (car class_env)))
(define class-bodies (lambda (class_env) (car (cdr class_env)) ) )
(define first-class-name (lambda (env) (caar env)))
(define first-class-info (lambda (env) (car (class-bodies env))))
(define first-class-body (lambda (env) (cadr (first-class-info env))))
(define addWorkingEnv (lambda (class_env) (cons (newenv) (cons class_env '()))))
(define the-working-env (lambda (env) (car env)))
(define class-definitions (lambda (env) (cadr env))) ;use when working with a class_env with a working env on (remember this for testing too so add worklayer)
(define pop-working-env-from-class-definitions (lambda (env) (cdr env)))
(define update-by-popping-off-working-env (lambda (working-env env) (cons working-env (pop-working-env-from-class-definitions env))))

(define insert-in-working-env
  (lambda (var val env)
    (cond
      ((box? env) (insert var val env))
      (else (update-by-popping-off-working-env (insert var val (the-working-env env)) env)))))
     
(define contains-in-working-env
  (lambda (var env)
    (cond
      ((box? env) (contains var env))
      (else (contains var (the-working-env env))))))

(define update-in-working-env
  (lambda (var val env)
    (cond
      ((box? env) (update var val env))
      (else (update var val (the-working-env env))))))

(define lookup-in-working-env
  (lambda (var env)
    (cond
      ((box? env) (lookup var env))
      (else (lookup var (the-working-env env))))))
      
(define lookup-in-class
  (lambda (var class-name class-env-with-working-layer)
    (lookup-in-class-helper var class-name (class-definitions class-env-with-working-layer))))

;after removing the working layer can just check in specific class
(define lookup-in-class-helper
  (lambda (var class-name env)
    (lookup var (class-body-of class-name env))))

;does NOT take env with working layer on top, pass in (class-definitions working-layer-on-top-class-env)
(define class-body-of
  (lambda (class-name env)
    (cond
      ((null? (car env)) (error "class does not exist"))
      ((eq? class-name (first-class-name env)) (first-class-body env))
      (else (class-body-of class-name
                           (cons (cdr (class-names env))
                                 (cons (cdr (class-bodies env))
                                       '())))))))

(define insert-class
  (lambda (class-name class-parent class-body class-env)
    (cons (cons class-name (class-names class-env))
          (cons (cons (cons class-parent (cons class-body '())) (class-bodies class-env)) '())) ))

(define class-instance-fields
  (lambda (class-name class-env-with-working-layer)
    (instance-fields (class-body-of class-name (class-definitions class-env-with-working-layer)))))

(define is-function-in-class
  (lambda (class-name function-name class-env-with-working-layer)
    (and (contains function-name (class-body-of class-name (class-definitions class-env-with-working-layer)))
          (not (containsInState function-name (class-instance-fields class-name class-env-with-working-layer))))))
  