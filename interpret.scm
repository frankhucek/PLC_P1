; Frank Hucek, Joseph Volpe, Robert Milne
(load "simpleParser.scm")
(load "state.scm")

; atom?
; returns whether or not the given param is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define interpret
  (lambda (filename)
    (interpret-parse-tree (parser filename) newState) ))

(define interpret-parse-tree
  (lambda (parsetree state)
    (cond
      ((null? parsetree) state)
      (else (interpret-parse-tree (cdr parsetree) (execute-statement (car parsetree) state))) )))

(define operator (lambda (statement) (if (null? statement) null (car statement))))
(define operand1 (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define operand2 (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define operand3 (lambda (statement) (if (null? (cdr (cdr (cdr statement)))) null (car (cdr (cdr (cdr statement))))) ))

(define execute-statement ; M_statement
  (lambda (statement state)
    ((eq? 'var (operator statement)) (execute-declaration statement state))
    ((eq? '= (operator statement)) (execute-assignment statement state))
    ((eq? 'return (operator statement)) (execute-return statement state))
    ((eq? 'if (operator statement)) (execute-conditional statement state))
    ((eq? 'while (operator statement)) (execute-while statement state))
    (else (execute-boolean-statement statement state))))

(define execute-declaration
  (lambda (statement state)
    (if (null? (operand2 statement))
        (insert state (operand1 statement) null)
        (insert state (operand1 statement) (execute-value-statement (operand2 statement) state)))))

(define execute-assignment
  (lambda (statement state)
    (update state (operand1 statement) (execute-value-statement (operand2 statement) state))))

(define execute-return
  (lambda (statement state)
    (execute-value-statement (operand1 statement) state)))

(define execute-conditional
  (lambda (statement state)
    (if (execute-boolean-statement (operand1 statement) state) ;boolean condition 
        (execute-statement (operand2 statement) state)      ;if true
        (execute-statement (operand3 statement) state))))   ;else false

(define execute-boolean-statement
  (lambda (statement state)
    (cond
      ((eq? '== (operator statement)) (= (execute-value-statement (operand1 statement) state)
                                         (execute-value-statement (operand2 statement) state)))
      ((eq? '!= (operator statement)) (not (= (execute-value-statement (operand1 statement) state)
                                              (execute-value-statement (operand2 statement) state))))
      ((eq? '< (operator statement)) (< (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '> (operator statement)) (> (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '<= (operator statement)) (<= (execute-value-statement (operand1 statement) state)
                                          (execute-value-statement (operand2 statement) state)))
      ((eq? '>= (operator statement)) (>= (execute-value-statement (operand1 statement) state)
                                          (execute-value-statement (operand2 statement) state)))
      ((eq? '&& (operator statement)) (and (execute-boolean-statement (operand1 statement) state)
                                           (execute-boolean-statement (operand2 statement) state)))
      ((eq? '|| (operator statement)) (or (execute-boolean-statement (operand1 statement) state)
                                          (execute-boolean-statement (operand2 statement) state)))
      ((eq? '! (operator statement)) (not (execute-boolean-statement (operand1 statement) state)))
      (else (execute-value-statement statement state)))))

(define execute-value-statement
  (lambda (statement state)
    (cond
      ((null? statement) statement)
      ((number? statement) statement) ;number 
      ((atom? statement) (lookup statement state)) ;variable
      ;should be a list, therefore a value statement with an operator and operands
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '- (operator statement)) (- (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '* (operator statement)) (* (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '/ (operator statement)) (/ (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '% (operator statement)) (% (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state))))))
    







  
                                     



    