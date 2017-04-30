; Frank Hucek, Joseph Volpe, Robert Milne
(load "stack.scm")

(define newenv (lambda () (box '( ((() ())) )  )))
(define addStackLayer (lambda (layer env) (begin (set-box! env (cons layer (unbox env))) env)))
(define pushEmptyStack (lambda (env) (addStackLayer (newStack) env)))
(define pushEmptyStateOnTopStack (lambda (env) (begin (set-box! env (cons (cons (newState) (peek env)) (unbox (pop env)))) env)))
(define peek (lambda (env) (car (unbox env))))
(define peekState (lambda (env) (car (peek env))))
(define pop (lambda (env) (begin (set-box! env (cdr (unbox env))) env)))
(define popState (lambda (env) (begin (set-box! env (cons (cdr (peek env)) (unbox (pop env)))) env)))
(define globalStack (lambda (env) (globalStackFinder (unbox env))))
(define globalStackFinder
  (lambda (env)
    (cond
      ((null? (cdr env)) (car env))
      (else (globalStackFinder (cdr env))))))
(define first-value (lambda (l) (car l)))
(define end-of-list (lambda (l) (cdr l)))
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

;update
(define update
  (lambda (name value env)
    (cond
      ((containsInStack name (peek env)) (addStackLayer (updateStack name value (peek env)) (pop env)))
      ((containsInStack name (globalStack env)) (begin (set-box! env (update-global-stack name value (unbox env))) env))
      ;(else (error "variable not in the current scope")))))

      (else (begin (set-box! env (update-helper name value (unbox env))) env)) )))

(define update-helper
  (lambda (name value unboxed-env)
    (cond
      ((null? (end-of-list unboxed-env)) (error "value not in env"))
      ((containsInStack name (first-value unboxed-env)) (cons (updateStack name value (first-value unboxed-env)) (end-of-list unboxed-env)))
      (else (cons (first-value unboxed-env) (update-helper name value (end-of-list unboxed-env)))) )))

(define update-global-stack
  (lambda (name value env)
    (cond
      ((null? (end-of-list env)) (cons (updateStack name value (first-value env)) '()))
      (else (cons (first-value env) (update-global-stack name value (end-of-list env)))) )))
    
;insert
(define insert
  (lambda (name value env)
    (addStackLayer (insertToStack name value (peek env)) (pop env)) ))
     
;removeItem
(define removeItem
  (lambda (name env)
    (cond
      ((containsInStack name (peek env)) (addStackLayer (removeItemFromStack name (peek env)) (pop env)))
      (else (error "variable not in the current scope")))))                                    
      
;lookup
(define lookup
  (lambda (name env)
    (cond
      ((containsInStack name (peek env)) (lookupInStack name (peek env)))
      ((containsInGlobal name env) (lookupInStack name (globalStack env)))
      (else (lookup-helper name (end-of-list (unbox env)))) )))

(define lookup-helper
  (lambda (name unboxed-env)
    (cond
      ((null? (end-of-list unboxed-env)) (error "value not in env"))
      ((containsInStack name (first-value unboxed-env)) (lookupInStack name (first-value unboxed-env)))
      (else (lookup-helper name (end-of-list unboxed-env))) )))
                
;contains in the top stack/current scope
(define contains
  (lambda (name env)
    (containsInStack name (peek env)) ))

(define containsInGlobal
  (lambda (name env)
    (containsInStack name (globalStack env)) ))

(define instance-fields
  (lambda (env)
    (instance-fields-helper (car (car (unbox env))))))

;'((main y x) ((() ((var a (new A)) (return (+ (dot a x) (dot a y))))) 10 5)))

(define instance-fields-helper
  (lambda (unboxed-env)
    (cond
      ((null? (firstValue unboxed-env)) unboxed-env)
      ((atom? (firstValue unboxed-env))
       (cons (cons (firstVariable unboxed-env)
                   (car (instance-fields-helper (combineVariablesAndValues (variablesAfterTheFirst unboxed-env) (valuesAfterTheFirst unboxed-env)))))
             (cons (cons (firstValue unboxed-env)
                         (cadr (instance-fields-helper (combineVariablesAndValues (variablesAfterTheFirst unboxed-env) (valuesAfterTheFirst unboxed-env)))))
                   '())))  
      (else (instance-fields-helper (combineVariablesAndValues (variablesAfterTheFirst unboxed-env) (valuesAfterTheFirst unboxed-env)))) )))