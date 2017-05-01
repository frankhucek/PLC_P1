; Frank Hucek, Joseph Volpe, Robert Milne
(load "classParser.scm")
(load "class_env.scm")

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
(define functionNameFromDotOperator (lambda (statement) (operand2 (functionName statement))))
(define functionParameters (lambda (statement) (operand2 statement)))
(define functionBody (lambda (statement) (operand3 statement)))
(define functionDefinition (lambda (statement) (cadr statement)))
(define paramsPassingIn (lambda (statement) (cddr statement)))
(define throw-value (lambda (statement) (if (null? (cdr statement)) null (car (cdr statement))) ))
(define invalid-break (lambda (v) (error "can only break in while")))
(define invalid-continue (lambda (v) (error "can only continue in a while")))
(define invalid-throw (lambda (v1 v2) (error "can only throw in a try and function")))
(define class-name (lambda (statement) (car statement)))
(define class-parent (lambda (statement) (cadr statement)))
(define class-body (lambda (statement) (caddr statement)))
(define instance-variables-from-left-side (lambda (statement) (cadr statement)))
(define class-type-from-left-side (lambda (statement) (car statement)))
(define classNameFromDotOperator (lambda (statement) (operand1 (functionName statement))))
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define interpret
  (lambda (filename classname)
    (execute-function-call '(funcall (dot classname main)) classname (addWorkingEnv (interpret-classes filename (new-class-env))) invalid-throw)))

(define interpret-classes
  (lambda (filename env)
    (interpret-parse-tree (parser filename) env exit invalid-break invalid-continue invalid-throw)))
      
(define interpret-parse-tree
  (lambda (parsetree stack exit break cont throw)
    (cond
      ((null? parsetree) stack)
      (else (interpret-parse-tree (rest-of-statements parsetree)
                                  (execute-statement (first-statement parsetree) stack exit break cont throw)
                                  ;(begin (execute-statement (first-statement parsetree) stack exit break cont throw) stack)
                                  exit break cont throw)) )))

(define execute-statement ; M_statement
  (lambda (statement stack exit break cont throw)
    (cond
      ((eq? 'class (operator statement))    (interpret-class (rest-of-statements statement) stack))
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

(define execute-begin
  (lambda (statement stack exit break cont throw)
    (cond
      ((null? statement) (popState stack))
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
               (pushEmptyStateOnTopStack (execute-begin (try-block statement) stack exit break cont
                                              (lambda (throw-value passed-stack) (valid-throw (execute-catch-block throw-value (catch-block statement) (pop passed-stack) exit break cont throw)))))))))

(define execute-try-block-with-finally
  (lambda (statement stack exit break cont throw)
    (call/cc
     (lambda (valid-throw)
               (execute-begin (finally-statements statement)
                              (pushEmptyStateOnTopStack (execute-begin (try-block statement) (pushEmptyStateOnTopStack stack) exit break cont
                                                             (lambda (throw-value passed-stack) (valid-throw (execute-begin (finally-statements statement)
                                                                                                                            (execute-catch-block throw-value (catch-block statement) (pop passed-stack) exit break cont throw)
                                                                                                                            exit break cont throw))
                                                             ))) exit break cont throw)))))

(define execute-catch-block
  (lambda (thrown-val statement stack exit break cont throw)
    (popState (execute-begin (catch-statements statement) (insert (catch-value-caught statement) thrown-val (pushEmptyStateOnTopStack (pushEmptyStateOnTopStack stack))) exit break cont throw)) ))

(define execute-declaration
  (lambda (statement stack throw)
    (cond
      ((contains-in-working-env (operand1 statement) stack) (error "No redefining variables"))
      ((null? (operand2 statement)) (insert-in-working-env (operand1 statement) null stack))
      (else (insert-in-working-env (operand1 statement) (execute-boolean-statement (operand2 statement) stack throw) stack)))))
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
      ((eq? 'new (operator statement)) (create-new-instance (operand1 statement) stack))
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
      ((eq? 'static-function (operator statement)) (save-function-definition  statement env))
      ((eq? 'funcall  (operator statement)) (handle-function-call statement env throw)) 
      ;should be a list, therefore either a dot operator, or value statement with an operator and operands
      ((eq? 'dot (operator statement)) (lookupInState (operand2 statement) (instance-variables-from-left-side (left-side-dot-expr statement env))))
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

;passed in (funcall (dot x foobar) 10 12) where 10 and 12 are parameters passed into function foobar called from class x
(define handle-function-call
  (lambda (statement env throw)
    (execute-function-call statement
                           (classNameFromDotOperator statement)
                           env
                           throw) ))
  
(define execute-function-call
  (lambda (statement class-name env throw)
    (let ([return-value (interpret-function (functionDefinition (lookup-in-class (functionNameFromDotOperator statement) class-name env))
                                   (cons (addStackLayer
                                    (set-parameters
                                     (paramsPassingIn statement) ;parameters your passing in e.g. 1, 2, 3 in foo(1, 2, 3)
                                     (first-statement (lookup-in-class (functionNameFromDotOperator statement) class-name env)) ;variables the values will be assigned to
                                     (newStack)
                                     (the-working-env env)
                                     )
                                    (the-working-env env))
                                         (cons (class-definitions env) '()))
                                   throw)])
        (pop (the-working-env env))
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

;will be passed in something like '(A () ((var x 5) (var y 10) (static-function main () ((var a (new A)) (return (+ (dot a x) (dot a y)))))))
;reads the class and inserts it into the class definitions in the env
(define interpret-class
  (lambda (statement env)
    (insert-class (class-name statement)
                  (class-parent statement)
                  (interpret-parse-tree (class-body statement) (newenv) exit invalid-break invalid-continue invalid-throw)
                  env) ))

;The instance must store the instance's class (i.e. the run-time type or the true type) and a list of instance field values.
;returns an instance with the structure (classname ((a b) (1 2))) where classname == class-name, a&b are instance fields, 1&2 respective values
(define create-new-instance
  (lambda (class-name env)
    (cons class-name
          (cons (class-instance-fields class-name env)
                '()))))

;statement -> (dot a y)
;env -> (working-env/box class-definitions)
;if in working-env there exists an instance of 'X of class X where there are no instance variables, then there is kept '(X (() ()))
;   therefore this returns '(X (() ()))
(define left-side-dot-expr
  (lambda (statement env)
    (lookup-in-working-env (operand1 statement) env)))
    
#|
All M_state and M_value functions will need to pass a parameter for the class-type (i.e. the compile-time type or current type)

Change your code for a variable to first lookup the variable in the local environment and if that fails to look in the non-static fields.

Update the code that interprets an assignment statement so that it
looks for the variables with dots in the instance fields
and for variables without dots it first looks in the local environment
and then in the instance fields.

Now test on the first 6 sample programs.
|#

