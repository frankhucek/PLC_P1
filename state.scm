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
(define variablesAfterTheFirst (lambda (state) (if (null? (variables state)) null (cdr (assignedValues state)))))
(define valuesAfterTheFirst (lambda (state) (if (null? (assignedValues state)) null (cdr (assignedValues state)))))
  
; ((x y z) (1 3 5)) -> ((a x y z) (13 1 3 5))
(define update
   (lambda (name value state)
     (insert name value (removeItem name state))))

(define insert
  (lambda (name value state)
    (cons (cons name (variables state))
          (cons (cons value (assignedValues state)) '() ) )) )

(define removeItem
  (lambda (name state)
    (cond
      ((null? (variables state)) (error "variable not in state"))
      ((eq? name (firstVariable state)) (cons (cdar state)
                               (cons (valuesAfterTheFirst state) '())))
      (else (insert (caar state) (caadr state) (removeItem name (cons (cdar state)
                               (cons (valuesAfterTheFirst state) '()))))))))

(define lookup
  (lambda (name state)
    (cond
      ((null? (variables state)) (error "variable not in state"))
      ((eq? name (caar state)) (caadr state))
      (else (lookup name (cons (cdar state)
                               (cons (valuesAfterTheFirst state) '())))))))

(define contains
  (lambda (name state)
    (cond
      ((null? (variables state)) #f)
      ((eq? name (caar state)) #t)
      (else (contains name (cons (cdar state)
                                 (cons (cdr (cadr state)) '())))))))
