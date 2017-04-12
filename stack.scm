; Frank Hucek, Joseph Volpe, Robert Milne
(load "state.scm")

(define newStack (lambda () '((() ())) ))
(define addLayer (lambda (layer stack) (cons layer stack)))
(define pushEmptyState (lambda (stack) (addLayer '(() ()) stack) ))
(define peekStack (lambda (stack) (car stack)))
(define popStack (lambda (stack) (cdr stack)))

;update
(define updateStack
  (lambda (name value stack)
    (cond
      ((containsInState name (peekStack stack)) (addLayer (updateState name value (peekStack stack)) (popStack stack)))
      (else (addLayer (peekStack stack) (updateStack name value (popStack stack)))) )))

;insert
(define insertToStack
  (lambda (name value stack)
    (addLayer (insertToState name value (peekStack stack)) (popStack stack)) ))
     
;removeItem
(define removeItemFromStack
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (peekStack stack)) (addLayer (removeItemFromState name (peekStack stack)) (popStack stack)))
      (else (addLayer (peekStack stack) (removeItemFromStack name (popStack stack)))) )))                                    
      
;lookup
(define lookupInStack
  (lambda (name stack)
    (cond
      ((null? stack) (error "variable not in stack"))
      ((containsInState name (peekStack stack)) (lookupInState name (peekStack stack)))
      (else (lookupInStack name (popStack stack))) )))

;contains
(define containsInStack
  (lambda (name stack)
    (cond
      ((null? stack) #f)
      ((containsInState name (peekStack stack)) #t)
      (else (containsInStack name (popStack stack))) )))