; Frank Hucek, Joseph Volpe, Robert Milne

; State definition
; '(() ())
; ((x y z) (1 3 5))
; x->1, y->3
; lookup x and y

(define newState (lambda () '(() ()) ))
(define variables (lambda (state) (if (null? state) null (car state))))
(define assignedValues (lambda (state) (if (null? (cdr state)) null (car (cdr state)))))
(define firstVariable (lambda (state) (if (null? (variables state)) null (caar state))))
(define firstValue (lambda (state) (if (null? (assignedValues state)) null (caadr state))))
(define variablesAfterTheFirst (lambda (state) (if (null? (variables state)) null (cdr (variables state)))))
(define valuesAfterTheFirst (lambda (state) (if (null? (assignedValues state)) null (cdr (assignedValues state)))))
(define addNameToState (lambda (name state) (cons name (variables state))))
(define addValueToState (lambda (value state) (cons value (assignedValues state))))
(define addStateLayerAroundValues (lambda (assignedValues) (cons assignedValues '())))
(define combineVariablesAndValues (lambda (variables assignedValues) (cons variables (addStateLayerAroundValues assignedValues))))

; ((x y z) (1 3 5)) -> ((a x y z) (13 1 3 5))
(define updateState
   (lambda (name value state)
     (insertToState name value (removeItemFromState name state))))

(define insertToState
  (lambda (name value state)
    (combineVariablesAndValues (addNameToState name state) (addValueToState value state))))

(define removeItemFromState
  (lambda (name state)
    (cond
      ((null? (variables state)) (error "variable not in state"))
      ((eq? name (firstVariable state)) (combineVariablesAndValues (variablesAfterTheFirst state) (valuesAfterTheFirst state)))
      (else (insertToState (firstVariable state) (firstValue state)
                           (removeItemFromState name (combineVariablesAndValues (variablesAfterTheFirst state) (valuesAfterTheFirst state))))))))

(define lookupInState
  (lambda (name state)
    (cond
      ((null? (variables state)) (error "variable not in state"))
      ((eq? name (firstVariable state)) (firstValue state))
      (else (lookupInState name (combineVariablesAndValues (variablesAfterTheFirst state) (valuesAfterTheFirst state)))))))

(define containsInState
  (lambda (name state)
    (cond
      ((null? (variables state)) #f)
      ((eq? name (firstVariable state)) #t)
      (else (containsInState name (combineVariablesAndValues (variablesAfterTheFirst state) (valuesAfterTheFirst state)))))))
