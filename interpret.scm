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
    (cond
      ((eq? 'var (operator statement)) (execute-declaration statement state))
      ((eq? '= (operator statement)) (execute-assignment statement state))
      ((eq? 'return (operator statement)) (execute-return statement state))
      ((eq? 'if (operator statement)) (execute-conditional statement state))
      ((eq? 'while (operator statement)) (execute-while statement state))
      (else (execute-boolean-statement statement state)))))

(define execute-declaration
  (lambda (statement state)
    (cond
      ((contains (operand1 statement) state) error "No redefining variables")
      ((null? (operand2 statement)) (insert (operand1 statement) null state))
      (else (insert (operand1 statement) (execute-boolean-statement (operand2 statement) state) state)))))
                                          ; changed exec-bool from exec-val

(define execute-assignment
  (lambda (statement state)
    (update (operand1 statement) (execute-value-statement (operand2 statement) state) state)))

(define execute-return
  (lambda (statement state)
    (cond
      ((boolean? (execute-return* statement state)) (convertSchemeBoolean (execute-return* statement state)) )
      (else
       (execute-return* statement state))) ))
      
(define execute-return*
  (lambda (statement state)
    (execute-boolean-statement (operand1 statement) state)))

(define execute-conditional
  (lambda (statement state)
    (cond
      ((null? (operand3 statement))
       (cond
         ((execute-boolean-statement (operand1 statement) state) (execute-statement (operand2 statement) state))
         (else state)))
      (else
       (if (execute-boolean-statement (operand1 statement) state) ;boolean condition 
           (execute-statement (operand2 statement) state)      ;if true
           (execute-statement (operand3 statement) state))))))   ;else false

(define execute-while
  (lambda (statement state)
    (cond
      ((execute-boolean-statement (operand1 statement) state) (execute-while statement (execute-statement (operand2 statement) state)) ) ; apply new state to next iteration
      (else state) )))

(define execute-boolean-statement
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

(define execute-value-statement
  (lambda (statement state)
    (cond
      ((null? statement) statement)
      ((isABooleanWord? statement) (convertBooleanWord statement))
      ((number? statement) statement) ;number 
      ((atom? statement) (lookup statement state)) ;variable
      ;should be a list, therefore a value statement with an operator and operands
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '- (operator statement)) (handle-unary-sign statement state))
      ((eq? '* (operator statement)) (* (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      ((eq? '/ (operator statement)) (quotient (execute-value-statement (operand1 statement) state)
                                               (execute-value-statement (operand2 statement) state)))
      ((eq? '% (operator statement)) (remainder (execute-value-statement (operand1 statement) state)
                                                (execute-value-statement (operand2 statement) state))))))

(define handle-unary-sign
  (lambda (statement state)
    (cond
      ((null? (operand2 statement)) (* -1 (execute-value-statement (operand1 statement) state)))
      (else (- (execute-value-statement (operand1 statement) state)
               (execute-value-statement (operand2 statement) state))))))

(define isABooleanWord?
  (lambda (statement)
    (or (eq? statement 'true) (eq? statement 'false))))

(define convertBooleanWord
  (lambda (statement)
    (cond
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      (else error "not a boolean"))))

(define convertSchemeBoolean
  (lambda (statement)
    (if statement 'true 'false)))

    