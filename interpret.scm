; Frank Hucek, Joseph Volpe, Robert Milne
(load "simpleParser.scm")
(load "state.scm")

(define interpret
  (lambda (filename)
    (interpret-parse-tree (parser filename) newState) ))

(define interpret-parse-tree
  (lambda (parsetree state)
    (cond
      ((null? parsetree) state)
      (else (interpret-parse-tree (cdr parsetree) (execute-statement (car parsetree) state))) )))


(define operand1

(define execute-statement ; M_statement
  (lambda (statement state)
    ((eq? 'var (operator statement)) (execute-declaration statement state))
    ((eq? '= (operator statement)) (execute-assignment statement state))
    ((eq? '= (operator statement)) (execute-assignment statement state))
    ((eq? '= (operator statement)) (execute-assignment statement state))
    ((eq? '= (operator statement)) (execute-assignment statement state)) ))
    