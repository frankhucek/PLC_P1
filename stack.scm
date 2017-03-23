; Frank Hucek, Joseph Volpe, Robert Milne
(load "state.scm")

(define newStack (lambda () '((() ())) ))
(define testStack '(((a b) (1 2)) ((x y) (3 4))) )

(define push
  (lambda (stack)
    (cons '(() ()) stack) ))
         
(define pop
  (lambda (stack)
    (car stack)))

(define removeTopLayer
  (lambda (stack)
    (cdr stack)))

;update
(define update
  (lambda (name value stack)
    (cond
      ((containsInState name (pop stack)) (cons (updateState name value (pop stack)) (removeTopLayer stack)))
      (else (cons (pop stack) (update name value (removeTopLayer stack)))) )))

;insert
(define insert
  (lambda (name value stack)
    (cons (insertToState name value (pop stack)) (removeTopLayer stack)) ))
     
;removeItem
(define removeItem
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (pop stack)) (cons (removeItemFromState name (pop stack)) (removeTopLayer stack)))
      (else (cons (pop stack) (removeItem name (removeTopLayer stack)))) )))                                    
      
;lookup
(define lookup
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (pop stack)) (lookupInState name (pop stack)))
      (else (lookup name (removeTopLayer stack))) )))

;contains
(define contains
  (lambda (name stack)
    (cond
      ((null? stack) #f)
      ((containsInState name (pop stack)) #t)
      (else (contains name (removeTopLayer stack))) )))