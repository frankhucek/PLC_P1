; Frank Hucek, Joseph Volpe, Robert Milne
(load "stack.scm")

(define testenv (box '( (((a b) (1 2))  ((c d) (3 4)))    (((e f) (5 6))  ((g h) (7 8)))    (((j k) (9 10))) )  ))
(define newenv (lambda () (box '( ((() ())) )  )))
(define addStackLayer (lambda (layer env) (begin (set-box! env (cons layer (unbox env))) env)))
(define pushEmptyStack (lambda (env) (addStackLayer (newStack) env)))
(define peekTopStack (lambda (env) (car (unbox env))))
(define popTopStack (lambda (env) (begin (set-box! env (cdr (unbox env))) env)))
(define globalStack (lambda (env) (globalStackFinder (unbox env))))
(define globalStackFinder
  (lambda (env)
    (cond
      ((null? (cdr env)) (car env))
      (else (globalStackFinder (cdr env))))))

;update
(define update
  (lambda (name value env)
    (cond
      ((containsInStack name (peekTopStack env)) (addStackLayer (updateStack name value (peekTopStack env)) (popTopStack env)))
      ((containsInStack name (globalStack env)) (insert name value env))
      (else (error "variable not in the current scope")))))

;insert
(define insert
  (lambda (name value env)
    (addStackLayer (insertToStack name value (peekTopStack env)) (popTopStack env)) ))
     
;removeItem
(define removeItem
  (lambda (name env)
    (cond
      ((containsInStack name (peekTopStack env)) (addStackLayer (removeItemFromStack name (peekTopStack env)) (popTopStack env)))
      (else (error "variable not in the current scope")))))                                    
      
;lookup
(define lookup
  (lambda (name env)
    (cond
      ((containsInStack name (peekTopStack env)) (lookupInStack name (peekTopStack env)))
      (else (lookup name (insert name (lookupInStack name (globalStack env)) env))))))

;contains in the top stack/current scope
(define contains
  (lambda (name env)
    (containsInStack name (peekTopStack env)) ))

(define containsInGlobal
  (lambda (name env)
    (containsInStack name (globalStack env)) ))