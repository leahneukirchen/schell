(define-syntax def
  (syntax-rules ()
    ((_ . rest)
     (define . rest))))

(define-syntax fn
  (syntax-rules ()
    ((_ . rest)
     (lambda . rest))))

(define-syntax if
  (syntax-rules ()
    ((_ test then)
     (%if test then))
    ((_ test then . else)
     (%if test then (begin . else)))))

(define-syntax when
  (syntax-rules ()
    ((_ test . body)
     (if test (begin . body)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test . body)
     (when (not test) . body))))

(define-syntax while
  (syntax-rules ()
    ((_ cond . rest)
     (%do ()
         ((not cond) '()) . rest))))

(define-syntax do
  (syntax-rules ()
    ((_ . rest)
     (begin . rest))))

(define-syntax do1
  (syntax-rules ()
    ((_ form1 . rest)
     (let ((r form1)) (begin . rest) r))))

;; clojure style let
(define-syntax with
  (syntax-rules ()
    ((_ (x y) . rest)
     (let ((x y)) . rest))
    ((_ (x y . brest) . rest)
     (let ((x y)) (with brest . rest)))
     ))

#|
(define-syntax for
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'for-each)
       (,(rename 'lambda)
        ,(list (cadr expr))
        ,@(cdddr expr))
       ,(caddr expr)))))
|#

(define-syntax for
  (syntax-rules ()
    ((_ x xs code ...)
     (for-each (lambda (x) code ...) xs))))

(def (str . args)
  (call-with-output-string (fn (out)
    (for-each (fn (x) (display x out)) args))))

(def (concat . args)
   (concatenate args))

(def (pp . args)
  (for-each (fn (x) (show #t (pretty x))) args))

(def (puts . args)
  (if (null? args)
      (newline)
      (for-each (fn (x)
                    (display x)
                    (unless (and (string? x) (string-suffix? "\n" x))
                      (newline)))
                args)))

(def (const x)
   (fn rest
      x))

(def (butlast list)
   (drop-right list 1))

(def (chomp s)
  (with (l (string-length s))
    (if (zero? l)
      s
      (substring s 0 (- l (if (= (string-ref s (- l 1)) #\newline)
                              1
                              0))))))

(def (range x . o)
  (with (start (if (pair? o) x 0)
         stop (if (pair? o) (car o) x)
         step (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  1)
         cmp (if (> step 0) >= <=))
     (let loop ((i start) (res '()))
       (if (cmp i stop)
           (reverse! res)
           (loop (+ i step) (cons i res))))))

(define-syntax with-return
  (syntax-rules ()
    ((_ return body ...)
     ;; call/ec would be sufficient if chibi had it
     (call-with-current-continuation (fn (return) body ...)))))
