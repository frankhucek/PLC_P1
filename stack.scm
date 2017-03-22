; Frank Hucek, Joseph Volpe, Robert Milne
(load "state.scm")

(define newStack (lambda () '((() ())) ))
                   
(define add
  (lambda (stack)
    (cons '(() () stack)) ))
         
(define pop
  (lambda (stack)
    (car stack)))

(define remove
  (lambda (stack)
    (cdr stack)))

;update

;insert
(define insert
  (lambda (name value stack)
    (insertToState

;removeItem

;lookup

;contains