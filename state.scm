; Frank Hucek, Joseph Volpe, Robert Milne

; State definition
; '(() ())
; ((x y z) (1 3 5))
; x->1, y->3
; lookup x and y

(define newState (lambda () '(() ()) ))

; ((x y z) (1 3 5)) -> ((a x y z) (13 1 3 5))
(define update
   (lambda (name value state)
     (insert name value (removeItem name state))))

(define insert
  (lambda (name value state)
    (cons (cons name (car state))
          (cons (cons value (car (cdr state))) '() ) )) )

(define removeItem
  (lambda (name state)
    (cond
      ((null? (car state)) (error "variable not in state"))
      ((eq? name (caar state)) (cons (cdar state)
                               (cons (cdr (car (cdr state))) '())))
      (else (insert (caar state) (caadr state) (removeItem name (cons (cdar state)
                               (cons (cdr (car (cdr state))) '()))))))))

(define lookup
  (lambda (name state)
    (cond
      ((null? (car state)) (error "variable not in state"))
      ((eq? name (caar state)) (caadr state))
      (else (lookup name (cons (cdar state)
                               (cons (cdr (car (cdr state))) '())))))))

(define contains
  (lambda (name state)
    (cond
      ((null? (car state)) #f)
      ((eq? name (caar state)) #t)
      (else (contains name (cons (cdar state)
                                 (cons (cdr (cadr state)) '())))))))
