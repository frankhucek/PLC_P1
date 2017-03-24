; Frank Hucek, Joseph Volpe, Robert Milne
(load "state.scm")

(define newStack (lambda () '((() ())) ))
(define addLayer (lambda (layer stack) (cons layer stack)))
(define pushEmptyState (lambda (stack) (addLayer '(() ()) stack) ))
(define peek (lambda (stack) (car stack)))
(define pop (lambda (stack) (cdr stack)))

;update
(define update
  (lambda (name value stack)
    (cond
      ((containsInState name (peek stack)) (addLayer (updateState name value (peek stack)) (pop stack)))
      (else (addLayer (peek stack) (update name value (pop stack)))) )))

;insert
(define insert
  (lambda (name value stack)
    (addLayer (insertToState name value (peek stack)) (pop stack)) ))
     
;removeItem
(define removeItem
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (peek stack)) (addLayer (removeItemFromState name (peek stack)) (pop stack)))
      (else (addLayer (peek stack) (removeItem name (pop stack)))) )))                                    
      
;lookup
(define lookup
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (peek stack)) (lookupInState name (peek stack)))
      (else (lookup name (pop stack))) )))

;contains
(define contains
  (lambda (name stack)
    (cond
      ((null? stack) #f)
      ((containsInState name (peek stack)) #t)
      (else (contains name (pop stack))) )))