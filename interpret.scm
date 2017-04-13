; Frank Hucek, Joseph Volpe, Robert Milne
(load "functionParser.scm")
(load "environment.scm")

(define operator (lambda (statement) (if (null? statement) null (car statement))))
(define operand1 (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define operand2 (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define operand3 (lambda (statement) (if (null? (cdr (cdr (cdr statement)))) null (car (cdr (cdr (cdr statement))))) ))
(define first-statement (lambda (list-of-statements) (car list-of-statements)))
(define rest-of-statements (lambda (list-of-statements) (cdr list-of-statements)))
(define try-block (lambda (statement) (if (null? statement) null (car statement))))
(define catch-block (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define catch-statements (lambda (statement) (car (cdr (cdr statement)))))
(define catch-value-caught (lambda (statement) (car (operand1 statement))))
(define finally-block (lambda (statement) (if (null? (cdr (cdr statement))) null (car (cdr (cdr statement)))) ))
(define finally-statements (lambda (statement) (if (null? (cdr (cdr statement))) null (cadr (car (cdr (cdr statement))))) ))
(define functionName (lambda (statement) (operand1 statement)))
(define functionParameters (lambda (statement) (operand2 statement)))
(define functionBody (lambda (statement) (operand3 statement)))
(define functionDefinition (lambda (statement) (cadr statement)))
(define paramsPassingIn (lambda (statement) (cddr statement)))
(define throw-value (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define invalid-break (lambda (v) (error "can only break in while")))
(define invalid-continue (lambda (v) (error "can only continue in a while")))
(define invalid-throw (lambda (v1 v2) (error "can only throw in a try and function")))

; atom? returns whether or not the given param is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define interpret
  (lambda (filename)
    (execute-function-call '(funcall main) (interpret-global-statements filename (newenv)) invalid-throw)))
       
(define interpret-global-statements
  (lambda (filename env)
    (interpret-parse-tree (parser filename) env exit invalid-break invalid-continue invalid-throw)))
      
(define interpret-parse-tree
  (lambda (parsetree stack exit break cont throw)
    (cond
      ((null? parsetree) stack)
      (else (interpret-parse-tree (rest-of-statements parsetree)
                                  (begin (execute-statement (first-statement parsetree) stack exit break cont throw) stack)
                                  exit break cont throw)) )))

(define execute-statement ; M_statement
  (lambda (statement stack exit break cont throw)
    (cond
      ((eq? 'begin (operator statement))    (execute-begin (rest-of-statements statement) (pushEmptyStateOnTopStack stack) exit break cont throw))
      ((eq? 'break (operator statement))    (break (popState stack)))
      ((eq? 'continue (operator statement)) (cont (popState stack)))
      ((eq? 'try (operator statement))      (execute-try-block (rest-of-statements statement) stack exit break cont throw))
      ((eq? 'throw (operator statement))    (throw (execute-value-statement (throw-value statement) stack throw) stack))
      ((eq? 'var (operator statement))      (execute-declaration statement stack throw))
      ((eq? '= (operator statement))        (execute-assignment statement stack throw))
      ((eq? 'return (operator statement))   (exit (execute-return statement stack)))
      ((eq? 'if (operator statement))       (execute-conditional statement stack exit break cont throw))
      ((eq? 'while (operator statement))    (execute-while statement stack exit throw))
      (else                                 (execute-boolean-statement statement stack throw)))))
      ;checks if operator statement is a function

;when calling this in other functions make sure to pass in (pushEmptyStateOnTopStack stack)
(define execute-begin
  (lambda (statement stack exit break cont throw)
    (cond
      ((null? statement) stack)
      (else (execute-begin (rest-of-statements statement) (execute-statement (first-statement statement) stack exit break cont throw) exit break cont throw)) )))

(define execute-try-block
  (lambda (statement stack exit break cont throw)
    (cond
      ((null? (finally-block statement)) (execute-try-block-without-finally statement stack exit break cont throw))
      (else (execute-try-block-with-finally statement stack exit break cont throw)) )))
  
(define execute-try-block-without-finally
  (lambda (statement stack exit break cont throw)
    (call/cc
     (lambda (valid-throw)
               (execute-begin (try-block statement) (pushEmptyStateOnTopStack stack) exit break cont
                                              (lambda (throw-value passed-stack) (valid-throw (execute-catch-block throw-value (catch-block statement) passed-stack exit break cont throw))))))))

(define execute-try-block-with-finally
  (lambda (statement stack exit break cont throw)
    (call/cc
     (lambda (valid-throw)
               (execute-begin (finally-statements statement)
                              (pushEmptyStateOnTopStack (execute-begin (try-block statement) (pushEmptyStateOnTopStack stack) exit break cont
                                                             (lambda (throw-value passed-stack) (valid-throw (execute-begin (finally-statements statement)
                                                                                                                            (pushEmptyStateOnTopStack (execute-catch-block throw-value (catch-block statement) (pop passed-stack) exit break cont throw))
                                                                                                                            exit break cont throw))
                                                             ))) exit break cont throw)))))

(define execute-catch-block
  (lambda (thrown-val statement stack exit break cont throw)
    (popState (execute-begin (catch-statements statement) (insert (catch-value-caught statement) thrown-val (pushEmptyStateOnTopStack stack)) exit break cont throw)) ))

(define execute-declaration
  (lambda (statement stack throw)
    (cond
      ((contains (operand1 statement) stack) (error "No redefining variables"))
      ((null? (operand2 statement)) (insert (operand1 statement) null stack))
      (else (insert (operand1 statement) (execute-boolean-statement (operand2 statement) stack throw) stack)))))
                                          ; changed exec-bool from exec-val

(define execute-assignment
  (lambda (statement stack throw)
    (update (operand1 statement) (execute-value-statement (operand2 statement) stack throw) stack)))

(define execute-return
  (lambda (statement stack)
    (cond
      ((boolean? (execute-return* statement stack)) (convertSchemeBoolean (execute-return* statement stack)) )
      (else (execute-return* statement stack))) ))
      
(define execute-return*
  (lambda (statement stack)
    (execute-boolean-statement (operand1 statement) stack invalid-throw)))

(define execute-conditional
  (lambda (statement stack exit break cont throw)
    (cond
      ((execute-boolean-statement (operand1 statement) stack throw) (execute-statement (operand2 statement) stack exit break cont throw)) ;if true
      ((null? (operand3 statement)) stack) ;false but no else block
      (else (execute-statement (operand3 statement) stack exit break cont throw))))) ;false and else block to execute

(define execute-while
  (lambda (statement stack exit throw)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (conditional block stack)
                                 (if (execute-boolean-statement conditional stack throw)
                                     (loop conditional block
                                           (call/cc (lambda (cont) (execute-statement block stack exit break cont throw))))
                                     stack))))
               (loop (operand1 statement) (operand2 statement) stack)))) ))

(define execute-boolean-statement
  (lambda (statement stack throw)
    (cond
      ((atom? statement) (execute-value-statement statement stack throw))
      ((eq? '== (operator statement)) (= (execute-value-statement (operand1 statement) stack throw)
                                         (execute-value-statement (operand2 statement) stack throw)))
      ((eq? '!= (operator statement)) (not (= (execute-value-statement (operand1 statement) stack throw)
                                              (execute-value-statement (operand2 statement) stack throw))))
      ((eq? '< (operator statement)) (< (execute-value-statement (operand1 statement) stack throw)
                                        (execute-value-statement (operand2 statement) stack throw)))
      ((eq? '> (operator statement)) (> (execute-value-statement (operand1 statement) stack throw)
                                        (execute-value-statement (operand2 statement) stack throw)))
      ((eq? '<= (operator statement)) (<= (execute-value-statement (operand1 statement) stack throw)
                                          (execute-value-statement (operand2 statement) stack throw)))
      ((eq? '>= (operator statement)) (>= (execute-value-statement (operand1 statement) stack throw)
                                          (execute-value-statement (operand2 statement) stack throw)))
      ((eq? '&& (operator statement)) (and (execute-boolean-statement (operand1 statement) stack throw)
                                           (execute-boolean-statement (operand2 statement) stack throw)))
      ((eq? '|| (operator statement)) (or (execute-boolean-statement (operand1 statement) stack throw)
                                          (execute-boolean-statement (operand2 statement) stack throw)))
      ((eq? '! (operator statement)) (not (execute-boolean-statement (operand1 statement) stack throw)))
      (else (execute-value-statement statement stack throw)))))

(define execute-value-statement
  (lambda (statement env throw)
    (cond
      ((null? statement) statement)
      ((isABooleanWord? statement) (convertBooleanWord statement))
      ((number? statement) statement)  
      ((atom? statement) (lookup statement env))
      ((eq? 'function (operator statement)) (save-function-definition  statement env))
      ((eq? 'funcall  (operator statement)) (execute-function-call statement env throw))
      ;should be a list, therefore a value statement with an operator and operands
      ((null? (execute-value-statement (operand1 statement) env throw)) (error "Variable one is not assigned")) ;checks to see if operand one has a value
      ((eq? '- (operator statement)) (handle-unary-sign statement env)) ;handles the case where there might be negative sign
      ((null? (execute-value-statement (operand2 statement) env throw)) (error "Variable two is not assigned")) ;checks operand two for below operations
      ((eq? '+ (operator statement)) (+ (execute-value-statement (operand1 statement) env throw)
                                        (execute-value-statement (operand2 statement) env throw)))
      ((eq? '* (operator statement)) (* (execute-value-statement (operand1 statement) env throw)
                                        (execute-value-statement (operand2 statement) env throw)))
      ((eq? '/ (operator statement)) (quotient (execute-value-statement (operand1 statement) env throw)
                                               (execute-value-statement (operand2 statement) env throw)))
      ((eq? '% (operator statement)) (remainder (execute-value-statement (operand1 statement) env throw)
                                                (execute-value-statement (operand2 statement) env throw))))))

(define handle-unary-sign
  (lambda (statement stack)
    (cond
      ((null? (operand2 statement)) (* -1 (execute-value-statement (operand1 statement) stack invalid-throw)))
      ((null? (execute-value-statement (operand2 statement) stack invalid-throw)) (error "Variable two is not assigned"))
      (else (- (execute-value-statement (operand1 statement) stack invalid-throw)
               (execute-value-statement (operand2 statement) stack invalid-throw))))))

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

(define save-function-definition
  (lambda (statement env)
         (insert (functionName statement)
                 (cons (functionParameters statement) (cons (functionBody statement) '()))
                 env)))

(define interpret-function
  (lambda (statement env throw)
    (call/cc (lambda (exit)
               (interpret-parse-tree statement env exit invalid-break invalid-continue throw) ))))

(define execute-function-call
  (lambda (statement env throw)
    (let ([return-value (interpret-function (functionDefinition (lookup (functionName statement) env))
                                   (addStackLayer
                                    (set-parameters
                                     (paramsPassingIn statement) ;parameters your passing in e.g. 1, 2, 3 in foo(1, 2, 3)
                                     (first-statement (lookup (operand1 statement) env)) ;variables the values will be assigned to
                                     (newStack)
                                     env
                                     )
                                    env)
                                   throw)])
        (pop env)
        return-value) ))

(define set-parameters
  (lambda (values variables stack env)
    (cond
      ((and (null? values) (null? variables)) stack)
      ((not (eq? (length values) (length variables))) (error "invalid arguments"))
      ((or (null? (rest-of-statements values)) (null? (rest-of-statements variables)))
       (insertToStack (first-statement variables)
               (execute-value-statement (first-statement values) env invalid-throw)
               stack))
      (else
       (insertToStack (first-statement variables)
                      (execute-value-statement (first-statement values) env invalid-throw)
                      (set-parameters (rest-of-statements values) (rest-of-statements variables) stack env))) )))
      