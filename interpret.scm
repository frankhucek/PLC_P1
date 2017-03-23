; Frank Hucek, Joseph Volpe, Robert Milne
(load "simpleParser.scm")
(load "stack.scm")

(define operator (lambda (statement) (if (null? statement) null (car statement))))
(define operand1 (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define operand2 (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define operand3 (lambda (statement) (if (null? (cdr (cdr (cdr statement)))) null (car (cdr (cdr (cdr statement))))) ))

(define invalid-break (lambda (v) (error "can only break in while")))
(define invalid-continue (lambda (v) (error "can only continue in a while")))
(define invalid-throw (lambda (v1 v2) (error "can only throw in a try")))

; atom?
; returns whether or not the given param is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define interpret
  (lambda (filename)
    (call/cc (lambda (exit) (interpret-parse-tree (parser filename) (newStack) exit invalid-break invalid-continue invalid-throw))) ))

(define interpret-parse-tree
  (lambda (parsetree state exit break cont throw)
    (cond
      ((null? parsetree) state)
      (else (interpret-parse-tree (cdr parsetree) (execute-statement (car parsetree) state exit break cont throw) exit break cont throw)) )))

(define execute-statement ; M_statement
  (lambda (statement state exit break cont throw)
    (cond
      ((eq? 'begin (operator statement))  (execute-begin (cdr statement) (push state) exit break cont throw))
      ((eq? 'break (operator statement))  (break (removeTopLayer state)))
      ((eq? 'continue (operator statement)) (cont state))
      ((eq? 'try (operator statement))    (execute-try-block (cdr statement) state exit break cont throw))
      ((eq? 'catch (operator statement))  (execute-catch-block (cdr statement) statement state exit break cont throw))
      ((eq? 'throw (operator statement))  (throw (value (execute-value-statement (operand1 statement) state)) state))
      ((eq? 'var (operator statement))    (execute-declaration statement state))
      ((eq? '= (operator statement))      (execute-assignment statement state))
      ((eq? 'return (operator statement)) (exit (execute-return statement state)))
      ((eq? 'if (operator statement))     (execute-conditional statement state exit break cont throw))
      ((eq? 'while (operator statement))  (execute-while statement state exit throw))
      (else (execute-boolean-statement statement state)))))

(define execute-begin
  (lambda (statement state exit break cont throw)
    (cond
      ((null? statement) (removeTopLayer state))
      (else (execute-begin (cdr statement) (execute-statement (car statement) state exit break cont throw) exit break cont)) )))

;(((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5))) (catch (e) ((= x e))) (finally ((= x (+ x 100)))))

(define execute-try-block
  (lambda (statement state exit break cont throw)
    (cond
      ((null? (operand3 statement)) execute-try-block-without-finally statement state exit throw)
      (else execute-try-block-with-finally statement state exit throw) )))
  

(define execute-try-block-without-finally
  (lambda (statement state exit break cont throw)
    (call/cc (lambda (valid-throw)
               (removeTopLayer (execute-begin (operator1 statement) (push state) exit break cont
                                              (lambda (v1 v2) (valid-throw (execute-catch-block v1 (operand2 statement) v2 exit break cont throw)))))))))

(define execute-try-block-with-finally
  (lambda (statement state exit break cont throw)
    (call/cc (lambda (valid-throw)
               (execute-begin (operand3 statement)
                              (removeTopLayer (execute-begin (operator1 statement) (push state) exit break cont
                                                             (lambda (v1 v2) (valid-throw (excute-begin (operand3 statement) (execute-catch-block v1 (operand2 statement) v2 exit break cont throw)))))))))))

(define execute-catch-block
  (lambda (statement state exit break cont throw)
    (removeTopLayer (execute-begin statement exit break cont throw)) ))
     
(define execute-declaration
  (lambda (statement state)
    (cond
      ((contains (operand1 statement) state) (error "No redefining variables"))
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
      (else (execute-return* statement state))) ))
      
(define execute-return*
  (lambda (statement state)
    (execute-boolean-statement (operand1 statement) state)))

(define execute-conditional
  (lambda (statement state exit break cont throw)
    (cond
      ((execute-boolean-statement (operand1 statement) state) (execute-statement (operand2 statement) state exit break cont throw)) ;if true
      ((null? (operand3 statement)) state) ;false but no else block
      (else (execute-statement (operand3 statement) state exit break cont throw))))) ;false and else block to execute

(define execute-while
  (lambda (statement state exit throw)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (conditional block state)
                                 (if (execute-boolean-statement conditional state)
                                     (loop conditional block
                                           (call/cc (lambda (cont) (execute-statement block state exit break cont throw))))
                                     state))))
               (loop (operand1 statement) (operand2 statement) state)))) ))

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
      ((null? (execute-value-statement (operand1 statement) state)) (error "Variable one is not assigned")) ;checks to see if operand one has a value
      ((eq? '- (operator statement)) (handle-unary-sign statement state))
      ((null? (execute-value-statement (operand2 statement) state)) (error "Variable two is not assigned")) ;checks operand two
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) state)
                                        (execute-value-statement (operand2 statement) state)))
      
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
      ((null? (execute-value-statement (operand2 statement) state)) (error "Variable two is not assigned"))
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
    