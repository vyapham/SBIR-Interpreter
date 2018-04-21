#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;
;; Name: Vy Pham
;; ID: vyapham
;;

(define *function-table* (make-hash))
(for-each
  (lambda (function) (hash-set! *function-table*  (car function) (cadr function)))
    `(
      (+, +)
      (-, -)
      (*, *)
      (/, (lambda (a b) (/ a b)))
      (abs, abs)
      (acos, acos)
      (asin, asin)
      (atan, atan)
      (ceil, ceiling)
      (cos, cos)
      (sin, sin)
      (tan, tan)
      (exp, exp)
      (round, round)
      (floor, floor)
      (log, log)
      (log10, (lambda (a) (/ (log a) (log 10.0))))
      (log2, (lambda (a) (/ (log a) (log 2.0))))
      (sqrt, sqrt)
      (trunc, truncate)
      (=, equal?)
      (<> , (lambda (a b) (not (equal? a b))))
      (<=, <=)
      (>=, >=)
      (<, <)
      (>, >)
      (^, expt)
    )
)

(define *variable-table* (make-hash))
(
  for-each
    (lambda (variable) (hash-set! *variable-table* (car variable) (cadr variable)))
    `(
      (pi 3.141592653589793238462643383279502884197169399)
      (e 2.718281828459045235360287471352662497757247093)
    )
)

(define (statement-eval statement)
  (cond
    (
      (eqv? (car statement) 'dim)
      ;; dim-statement
      (dim-statement (car(cadr statement)) (cadr(cadr statement)) )
    )
    (
      (eqv? (car statement) 'let)
      ;; let-statement
      (let-statement (car(cdr statement)) (cadr(cdr statement)))
    )
    (
      (eqv? (car statement) 'goto)
      ;; goto-statement
      (goto-statement (cadr statement))
    )
    (
      (eqv? (car statement) 'if)
      ;; if-statement
      (if-statement (cadr statement) (caddr statement))
    )
    (
      (eqv? (car statement) 'print)
      ;; print-statement
      (if (vector? (cdr statement))
	  (print-statement (vector-ref (car(cdr statement)) (inexact->exact(-(expression-eval(cadr(cdr statement))) 1))))
	  (print-statement (cdr statement))
      )
      (printf "~n")
    )
    (
      (eqv? (car statement) 'input)
      ;; input-statement
      (hash-set! *variable-table* 'inputcount (length (cdr statement)))
      (input-statement (cdr statement))
    )
    (else
      (list? statement)
      (print-statement (cdr statement))
      (printf "~n")
    )
  )
)

(define (dim-statement var expression)
  (hash-set! *variable-table* var (make-vector (inexact->exact(expression-eval expression))))
)

(define (let-statement var expression)
  (cond
    (
      (symbol? var) (hash-set! *variable-table* var (expression-eval expression))
    )
    (
      (pair? var)
      (if  (<= (- (expression-eval (cadr var)) 1) (vector-length (hash-ref *variable-table* (car var))))
        (vector-set!
          (hash-ref *variable-table* (car var))
          (inexact->exact (- (expression-eval (cadr var)) 1) )
          (expression-eval expression)
        )
        (printf("Error!"))
      )
    )
  )
)

(define (goto-statement label)
(cond 
  (
    (not(null? (hash-ref *label-table* label)))
    (statement-eval (hash-ref label-table-2 label))
    (run-program (hash-ref *label-table* label))
  )
  )
)

(define (if-statement condition label)
  (when ((hash-ref *function-table* (car condition)) (expression-eval (car(cdr condition))) (expression-eval (cadr(cdr condition))) )
    (goto-statement label)
  )
)

(define (print-statement toPrint)
  (when (not (null? toPrint))
      (if (string? (car toPrint))
        (printf "~a " (car toPrint))
        (printf "~a " (expression-eval (car toPrint)))
      )
      (print-statement (cdr toPrint))
  )
)

(define (input-statement inputList)
  (cond 
   (
    (null? inputList)
    (hash-set! *variable-table* (car inputList) (read))
    (input-statement (cdr inputList))
   )
 )
)

(define (expression-eval expression)
  (cond
    ;; If symbol or number, convert to float by adding 0.0
    (
      (symbol? expression) (+ (hash-ref *variable-table* expression) 0.0)
    )
    (
      (number? expression) (+ expression 0.0)
    )
    (
    ;; If pair, check in function-table and run the function. If not in the table, that's a variable
      (pair? expression)
      (if (hash-has-key? *function-table* (car expression))
        (apply (hash-ref *function-table* (car expression)) (map expression-eval (cdr expression)) )
        (if (and (vector? (car expression)) (hash-has-key? *variable-table* (car expression)))
          (vector-ref 
            (hash-ref *variable-table* (car expression))
            (inexact->exact (- (expression-eval (cadr expression)) 1) )
          )
          (void)
        )
      )
    )
  )
)

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
      (map (lambda (line) (printf "~s~n" line)) program)
      (printf ")~n"
    )
)

(define *label-table* (make-hash))
(define label-table-2 (make-hash))

;; program ~ list
(define (make-labels program)
  (if (not(null? program))
    (if (= (length (car program)) 1)
      (make-labels (cdr program))
      ;; else if a symbol
      (cond 
        (
          (symbol? (cadr (car program) ))
          (hash-set! *label-table* (cadr (car program)) (cdr program) )
          (if (eqv? (cadr (car program)) (last (car program))) 
            (void)
            (hash-set! label-table-2 (cadr (car program)) (caddr(car program)))
          )
          (if (not(null? (cdr program))) (make-labels (cdr program)) (void))
        )
        (else
          (if (not(null? (cdr program)))
            (make-labels (cdr program))
            (void)
          )
        )
      )
    )
    (void)
  )
)

;; program ~ list
(define (run-program program)
  (if (not(null? program))
    (if (= (length (car program)) 1)
      (run-program (cdr program))
      (if (symbol? (cadr(car program)))
        (when (not(null? (caddr(car program))))
          (statement-eval (caddr(car program)))
	  (if (not(eqv? (car(caddr(car program))) 'goto))
            (run-program (cdr program) )
            (void)
          )
        )
        (when (list? (cadr(car program)))
          (statement-eval (cadr(car program)))
          (if (not(eqv? (car(cadr(car program))) 'goto))
            (run-program (cdr program))
            (void)
          )
        )
      )
    )
    (void)
  )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* (
                (sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile))
              )

	  ;;(write-program-by-line sbprogfile program)
              (make-labels program)
              (run-program program)
	 )
    )
)

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))
