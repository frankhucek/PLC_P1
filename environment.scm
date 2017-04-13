; Frank Hucek, Joseph Volpe, Robert Milne
(load "stack.scm")

(define testenv (box '( (((a b) (1 2))  ((c d) (3 4)))    (((e f) (5 6))  ((g h) (7 8)))    (((j k) (9 10))) )  ))
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
      ((null? (cdr unboxed-env)) (error "value not in env"))
      ((containsInStack name (car unboxed-env)) (cons (updateStack name value (car unboxed-env)) (cdr unboxed-env)))
      (else (cons (car unboxed-env) (update-helper name value (cdr unboxed-env)))) )))

(define update-global-stack
  (lambda (name value env)
    (cond
      ((null? (cdr env)) (cons (updateStack name value (car env)) '()))
      (else (cons (car env) (update-global-stack name value (cdr env)))) )))
    
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
      (else (lookup-helper name (cdr (unbox env)))) )))

(define lookup-helper
  (lambda (name unboxed-env)
    (cond
      ((null? (cdr unboxed-env)) (error "value not in env"))
      ((containsInStack name (car unboxed-env)) (lookupInStack name (car unboxed-env)))
      (else (lookup-helper name (cdr unboxed-env))) )))
                
;contains in the top stack/current scope
(define contains
  (lambda (name env)
    (containsInStack name (peek env)) ))

(define containsInGlobal
  (lambda (name env)
    (containsInStack name (globalStack env)) ))
