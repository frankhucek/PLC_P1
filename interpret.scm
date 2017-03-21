; Frank Hucek, Joseph Volpe, Robert Milne
(load "simpleParser.scm")
(load "state.scm")

; atom?
; returns whether or not the given param is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;sends the parsed file and a state to the interperter
(define interpret
  (lambda (filename)
    (interpret-parse-tree (parser filename) (newState)) ))

;moves through the statements inside of the parse tree
(define interpret-parse-tree
  (lambda (parsetree state)
    (cond
      ((null? parsetree) state)
      (else (interpret-parse-tree (cdr parsetree) (execute-statement (car parsetree) state))) )))

;points to the diffrent elements inside of a statement
(define operator (lambda (statement) (if (null? statement) null (car statement))))
(define operand1 (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define operand2 (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define operand3 (lambda (statement) (if (null? (cdr (cdr (cdr statement)))) null (car (cdr (cdr (cdr statement))))) ))

;takes statmenets and checks if the operator is a statment if it is it sends it to the appropriate method to handel it
;if it is not a statement it sends the statement to be evaluated as a boolean statement
(define execute-statement ; M_statement
  (lambda (statement state)
    (cond
      ((eq? 'var (operator statement)) (execute-declaration statement state))
      ((eq? '= (operator statement)) (execute-assignment statement state))
      ((eq? 'return (operator statement)) (execute-return statement state))
      ((eq? 'if (operator statement)) (execute-conditional statement state))
      ((eq? 'while (operator statement)) (execute-while statement state))
      (else (execute-boolean-statement statement state)))))

;handels decleration statements checking to make sure that the declared variable does not already exist
(define execute-declaration
  (lambda (statement state)
    (cond
      ((contains (operand1 statement) state) error "No redefining variables")
      ((null? (operand2 statement)) (insert (operand1 statement) null state))
      (else (insert (operand1 statement) (execute-boolean-statement (operand2 statement) state) state)))))
                                          ; changed exec-bool from exec-val

;handels assignment statements 
(define execute-assignment
  (lambda (statement state)
    (update (operand1 statement) (execute-value-statement (operand2 statement) state) state)))

;takes the return statement and checks to see if it is a boolean in scheme if it is converts it to english
(define execute-return
  (lambda (statement state)
    (cond
      ((boolean? (execute-return* statement state)) (convertSchemeBoolean (execute-return* statement state)) )
      (else (execute-return* statement state))) ))

;handles the return statments
(define execute-return*
  (lambda (statement state)
    (execute-boolean-statement (operand1 statement) state)))

;handels the if statements, checks to see if there is an else statement or it is just the if
(define execute-conditional
  (lambda (statement state)
    (cond
      ((execute-boolean-statement (operand1 statement) state) (execute-statement (operand2 statement) state)) ;if true
      ((null? (operand3 statement)) state) ;false but no else block
      (else (execute-statement (operand3 statement) state))))) ;false and else block to execute

;handels while loop statements
(define execute-while
  (lambda (statement state)
    (cond
      ((execute-boolean-statement (operand1 statement) state) (execute-while statement (execute-statement (operand2 statement) state)) ) ; apply new state to next iteration
      (else state) )))

;takes statmenets and checks if the operator is a boolean operator if it is it does the appropriate boolean math to the statement
;if it is not a boolean operator it sends the statement to be evaluated as a value statement
(define execute-boolean-statement ;M_boolean
  (lambda (statement state)
    (cond
      ((atom? statement) (execute-value-statement statement state))
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

;takes statmenets and checks if the operator is a mathmatical operator if it is it does the appropriate math to the statement
;this is also where the value of the objects gets returned, if it is a variable it returns the value of that variable
;if it is a english boolean word it converts it to schemes boolean symbols #t #f
(define execute-value-statement
  (lambda (statement state)
    (cond
      ((null? statement) statement)
      ((isABooleanWord? statement) (convertBooleanWord statement)) ;takes boolean english words and converts it to scheme
      ((number? statement) statement) ;returns numbers
      ((atom? statement) (lookup statement state)) ;gets the variables value
      ;should be a list, therefore a value statement with an operator and operands
      ((null? (execute-value-statement (operand1 statement) state)) error "Variable one is not defined") ;checks to see if operand one has a value
      ((null? (execute-value-statement (operand2 statement) state)) error "Variable two is not defined") ;checks operand two
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '- (operator statement)) (handle-unary-sign statement state))
      ((eq? '* (operator statement)) (* (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '/ (operator statement)) (quotient (execute-value-statement (operand1 statement) state)
                                               (execute-value-statement (operand2 statement) state)))
      ((eq? '% (operator statement)) (remainder (execute-value-statement (operand1 statement) state)
                                                (execute-value-statement (operand2 statement) state))))))

;handels the minus symbols checking to see if it symbolises a negative number or subtracting two numbers
(define handle-unary-sign
  (lambda (statement state)
    (cond
      ((null? (operand2 statement)) (* -1 (execute-value-statement (operand1 statement) state)))
      (else (- (execute-value-statement (operand1 statement) state)
               (execute-value-statement (operand2 statement) state))))))

;checks to see if the statement is a boolean word in english
(define isABooleanWord?
  (lambda (statement)
    (or (eq? statement 'true) (eq? statement 'false))))

;converts boolean words in english to scheme's booleans
(define convertBooleanWord
  (lambda (statement)
    (cond
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      (else error "not a boolean"))))

;converts scheme boolean's to english boolean words.
(define convertSchemeBoolean
  (lambda (statement)
    (if statement 'true 'false)))