; Frank Hucek, Joseph Volpe, Robert Milne
(load "simpleParser.scm")
(load "stack.scm")

(define operator (lambda (statement) (if (null? statement) null (car statement))))
(define operand1 (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define operand2 (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define operand3 (lambda (statement) (if (null? (cdr (cdr (cdr statement)))) null (car (cdr (cdr (cdr statement))))) ))
(define first-statement (lambda (list-of-statements) (car list-of-statements)))
(define rest-of-statements (lambda (list-of-statements) (cdr list-of-statements)))

(define invalid-break (lambda (v) (error "can only break in while")))
(define invalid-continue (lambda (v) (error "can only continue in a while")))
(define invalid-throw (lambda (v1 v2) (error "can only throw in a try")))

; atom? returns whether or not the given param is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define interpret
  (lambda (filename)
    (call/cc
     (lambda (exit)
       (interpret-parse-tree (parser filename) (newStack) exit invalid-break invalid-continue invalid-throw))) ))

(define interpret-parse-tree
  (lambda (parsetree stack exit break cont throw)
    (cond
      ((null? parsetree) stack)
      (else (interpret-parse-tree (rest-of-statements parsetree)
                                  (execute-statement (first-statement parsetree) stack exit break cont throw) exit break cont throw)) )))

(define execute-statement ; M_statement
  (lambda (statement stack exit break cont throw)
    (cond
      ((eq? 'begin (operator statement))    (execute-begin (rest-of-statements statement) (pushEmptyState stack) exit break cont throw))
      ((eq? 'break (operator statement))    (break (pop stack)))
      ((eq? 'continue (operator statement)) (cont stack))
      ((eq? 'try (operator statement))      (execute-try-block (rest-of-statements statement) stack exit break cont throw))
      ((eq? 'catch (operator statement))    (execute-catch-block (rest-of-statements statement) statement stack exit break cont throw))
      ((eq? 'throw (operator statement))    (throw (value (execute-value-statement (operand1 statement) stack)) stack))
      ((eq? 'var (operator statement))      (execute-declaration statement stack))
      ((eq? '= (operator statement))        (execute-assignment statement stack))
      ((eq? 'return (operator statement))   (exit (execute-return statement stack)))
      ((eq? 'if (operator statement))       (execute-conditional statement stack exit break cont throw))
      ((eq? 'while (operator statement))    (execute-while statement stack exit throw))
      (else                                 (execute-boolean-statement statement stack)))))

(define execute-begin
  (lambda (statement stack exit break cont throw)
    (cond
      ((null? statement) (pop stack))
      (else (execute-begin (rest-of-statements statement) (execute-statement (first-statement statement) stack exit break cont throw) exit break cont throw)) )))

(define execute-try-block
  (lambda (statement stack exit break cont throw)
    (cond
      ((null? (operand3 statement)) execute-try-block-without-finally statement stack exit throw)
      (else execute-try-block-with-finally statement stack exit throw) )))
  
(define execute-try-block-without-finally
  (lambda (statement stack exit break cont throw)
    (call/cc (lambda (valid-throw)
               (pop (execute-begin (operator1 statement) (pushEmptyState stack) exit break cont
                                              (lambda (v1 v2) (valid-throw (execute-catch-block v1 (operand2 statement) v2 exit break cont throw)))))))))

(define execute-try-block-with-finally
  (lambda (statement stack exit break cont throw)
    (call/cc (lambda (valid-throw)
               (execute-begin (operand3 statement)
                              (pop (execute-begin (operator1 statement) (pushEmptyState stack) exit break cont
                                                             (lambda (v1 v2) (valid-throw (excute-begin (operand3 statement) (execute-catch-block v1 (operand2 statement) v2 exit break cont throw)))))))))))

(define execute-catch-block
  (lambda (statement stack exit break cont throw)
    (pop (execute-begin statement exit break cont throw)) ))
     
(define execute-declaration
  (lambda (statement stack)
    (cond
      ((contains (operand1 statement) stack) (error "No redefining variables"))
      ((null? (operand2 statement)) (insert (operand1 statement) null stack))
      (else (insert (operand1 statement) (execute-boolean-statement (operand2 statement) stack) stack)))))
                                          ; changed exec-bool from exec-val

(define execute-assignment
  (lambda (statement stack)
    (update (operand1 statement) (execute-value-statement (operand2 statement) stack) stack)))

(define execute-return
  (lambda (statement stack)
    (cond
      ((boolean? (execute-return* statement stack)) (convertSchemeBoolean (execute-return* statement stack)) )
      (else (execute-return* statement stack))) ))
      
(define execute-return*
  (lambda (statement stack)
    (execute-boolean-statement (operand1 statement) stack)))

(define execute-conditional
  (lambda (statement stack exit break cont throw)
    (cond
      ((execute-boolean-statement (operand1 statement) stack) (execute-statement (operand2 statement) stack exit break cont throw)) ;if true
      ((null? (operand3 statement)) stack) ;false but no else block
      (else (execute-statement (operand3 statement) stack exit break cont throw))))) ;false and else block to execute

(define execute-while
  (lambda (statement stack exit throw)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (conditional block stack)
                                 (if (execute-boolean-statement conditional stack)
                                     (loop conditional block
                                           (call/cc (lambda (cont) (execute-statement block stack exit break cont throw))))
                                     stack))))
               (loop (operand1 statement) (operand2 statement) stack)))) ))

(define execute-boolean-statement
  (lambda (statement stack)
    (cond
      ((atom? statement) (execute-value-statement statement stack))
      ((eq? '== (operator statement)) (= (execute-value-statement (operand1 statement) stack)
                                         (execute-value-statement (operand2 statement) stack)))
      ((eq? '!= (operator statement)) (not (= (execute-value-statement (operand1 statement) stack)
                                              (execute-value-statement (operand2 statement) stack))))
      ((eq? '< (operator statement)) (< (execute-value-statement (operand1 statement) stack)
                                        (execute-value-statement (operand2 statement) stack)))
      ((eq? '> (operator statement)) (> (execute-value-statement (operand1 statement) stack)
                                        (execute-value-statement (operand2 statement) stack)))
      ((eq? '<= (operator statement)) (<= (execute-value-statement (operand1 statement) stack)
                                          (execute-value-statement (operand2 statement) stack)))
      ((eq? '>= (operator statement)) (>= (execute-value-statement (operand1 statement) stack)
                                          (execute-value-statement (operand2 statement) stack)))
      ((eq? '&& (operator statement)) (and (execute-boolean-statement (operand1 statement) stack)
                                           (execute-boolean-statement (operand2 statement) stack)))
      ((eq? '|| (operator statement)) (or (execute-boolean-statement (operand1 statement) stack)
                                          (execute-boolean-statement (operand2 statement) stack)))
      ((eq? '! (operator statement)) (not (execute-boolean-statement (operand1 statement) stack)))
      (else (execute-value-statement statement state)))))

(define execute-value-statement
  (lambda (statement stack)
    (cond
      ((null? statement) statement)
      ((isABooleanWord? statement) (convertBooleanWord statement))
      ((number? statement) statement)  
      ((atom? statement) (lookup statement stack)) 
      ;should be a list, therefore a value statement with an operator and operands
      ((null? (execute-value-statement (operand1 statement) stack)) (error "Variable one is not assigned")) ;checks to see if operand one has a value
      ((eq? '- (operator statement)) (handle-unary-sign statement stack)) ;handles the case where there might be negative sign
      ((null? (execute-value-statement (operand2 statement) stack)) (error "Variable two is not assigned")) ;checks operand two for below operations
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) stack)
                                        (execute-value-statement (operand2 statement) stack)))
      ((eq? '* (operator statement)) (* (execute-value-statement (operand1 statement) stack)
                                        (execute-value-statement (operand2 statement) stack)))
      ((eq? '/ (operator statement)) (quotient (execute-value-statement (operand1 statement) stack)
                                               (execute-value-statement (operand2 statement) stack)))
      ((eq? '% (operator statement)) (remainder (execute-value-statement (operand1 statement) stack)
                                                (execute-value-statement (operand2 statement) stack))))))

(define handle-unary-sign
  (lambda (statement stack)
    (cond
      ((null? (operand2 statement)) (* -1 (execute-value-statement (operand1 statement) stack)))
      ((null? (execute-value-statement (operand2 statement) stack)) (error "Variable two is not assigned"))
      (else (- (execute-value-statement (operand1 statement) stack)
               (execute-value-statement (operand2 statement) stack))))))

(define isABooleanWord?
  (lambda (statement)
    (or (eq? statement 'true) (eq? statement 'false))))

(define convertBooleanWord
  (lambda (statement)
    (cond
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      (else (error "not a boolean")))))

(define convertSchemeBoolean
  (lambda (statement)
    (if statement 'true 'false)))
    