; INTERPRETERS PROJECT PART 1
; Matthew Conley, Justin Ferri, Drake Poling

;load simpleParser into the Interpreter
(load "simpleParser.scm")

;interpret function that takes a file name and from the file makes the state
(define interpret
  (lambda (filename)
    (parser filename)))

;dont think this is right but i want to get the program to run files
(define run-interpreter
  (lambda (filename)
    (M_state (parser filename) '() )))

; Some definitions to be used relating to the state
(define varname caar)
(define varvalue cdar)
(define varpair car)
(define restof cdr)

(define M_state
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((eq? (caar expression) 'return) (M_value(cadar expression)))
      ((eq? '= (operator expression)) (M_assign (expression state)))
      (else (M_value expression)))))

(define M_lookup
  (lambda (var state)
    (cond
      ((null? state) state) ; return the empty state
      ((eq? var (varname state)) (varpair state))
      (else (error 'unknown "unknown variable")))))



; M_remove takes a variable and the state
(define M_remove
  (lambda (var state)
    (cond
      ((null? state) state) ; return the empty state
      ((eq? var (varname state)) (restof state))
      (else (cons (varpair state) (M_remove var (restof state))))))) 



; M_add takes a variable, its value, and the state
(define M_add
  (lambda (var value state)
    (cons (cons var (cons value '())) state)))



; M_assign takes a var, a value, and the state
; removes the var if it is in the state and then adds it to the state with its new value
(define M_assign
  (lambda (var value state)
    (M_add var value (M_remove var state))))
                      
(define M_unary
  (lambda (expression)
    ((eq? (caar expression) '-)
    ((eq? (caar expression) '!)))))

; M_value takes an expression in the form '(value operator value) and the state
; only 2 values and one operator are allowed
; evaluates the expression and returns the result
; uses abstraction for operator, operand1, and operand2
(define M_value
  (lambda (expr) ; WILL ALSO TAKE THE STATE
    (cond
      ((not (list? expr)) (M_atomvalue expr state))
      ; Mathematical Expressions
      ((eq? (operator expr) '+) (+ (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '-) (- (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '*) (* (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '/) (quotient (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '%) (remainder (M_value(operand1 expr)) (M_value(operand2 expr))))
      ; Comparison Expressions
      ((eq? (operator expr) '==) (eq? (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '!=) (not (eq? (M_value(operand1 expr)) (M_value(operand2 expr)))))
      ((eq? (operator expr) '>=) (>= (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '<=) (<= (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '>) (> (M_value(operand1 expr)) (M_value(operand2 expr))))
      ((eq? (operator expr) '<) (< (M_value(operand1 expr)) (M_value(operand2 expr))))
      (else (error 'unknown "invalid expression")))))

; we can change all of the definitions below and alter how the operators work
; prefix, postfix, in between the operators
(define operator cadr)

(define operand1 car)

(define operand2 caddr)

; Later this will check to see if the atom is a variable and if it is, return the value of it (without using M_value)
(define M_atomvalue
  (lambda (atom state)
    (cond
      ((number? atom) atom)
      (else (cadr (M_lookup atom state)))))) 

; M_return returns the value of the expression
;(define M_return
;  (lambda (expr state)
;    (
; WE ACTUALLY MIGHT NOT NEED THIS

; M_state
(define M_state-while
  (lambda (condition statement state)
    (if (M_boolean condition state)
        (M_state-while condition statement (M_state-while statement state))
        state)))

; so we also need to define M_boolean

