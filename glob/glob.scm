(define (glob pattern)
   ;; add options
   (let ((result (%glob pattern 0)))
     (if (zero? (car result))
         (glob-path-values (cadr result))
         ;; XXX error handling
         '())))
