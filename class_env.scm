; class environment
(load "environment.scm")

#|(define class_environment (lambda () (cons (newenv) '() ))) ; ((list of class_names) (list of environments))

(define testenv1 (box '( (((a b) (1 2))  ((c d) (3 4)))    (((e f) (5 6))  ((g h) (7 8)))    (((j k) (9 10))) )  ))
(define testenv2 (box '( (((l m) (1 2))  ((n o) (3 4)))    (((p q) (5 6))  ((r s) (7 8)))    (((t u) (9 10))) )  ))
(define test_class_env (cons testenv1 (cons testenv2 (class_environment) )))

(define addEnvLayer
  (lambda (env class_env)
    (cons env class_env)))

(define class_peek
  (lambda (class_env)
    (car class_env)))

(define class_pop
  (lambda (class_env)
    (cdr class_env)))

; insert, update, remove, lookup, contains

; insert
(define class_insert
  (lambda (name value class_env)
    (addEnvLayer (insert name value (class_peek class_env)) (class_pop class_env)) ))
|#

(define testenv1 (box '( (((a b) (1 2))  ((c d) (3 4)))    (((e f) (5 6))  ((g h) (7 8)))    (((j k) (9 10))) )  ))
(define testenv2 (box '( (((l m) (1 2))  ((n o) (3 4)))    (((p q) (5 6))  ((r s) (7 8)))    (((t u) (9 10))) )  ))

(define new_class_env (lambda () (cons '() (cons (cons (newenv) '() ) '() ))))
(define test_class_env (cons '(class1 class2) (cons (cons testenv1 (cons testenv2 '())) '())) )

(define class_names (lambda (class_env) (car class_env)))
(define class_bodies (lambda (class_env) (car (cdr class_env)) ) )

(define class_addEnvLayer
  (lambda (classname env class_env)
    (cons (cons classname (class_names class_env)) (cons (cons env (class_bodies class_env)) '()) )))

(define insert_into_class ; ((classnames) (envs)) -> (envs) => (env1 env2 env3) insert variable/value into the correct class's env
  (lambda (varname value class_env)
    (cons
     (class_names class_env); class names
     (cons (insert varname value (car (class_bodies class_env))) (cdr (class_bodies class_env))) ))) ; class bodies








