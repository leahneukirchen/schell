(define-syntax $
  (syntax-rules ()
    ((_ name)
     (getenv (str (quote name))))))

;; XXX add epf redirects etc
(def (run/str1 cmd . args)
  (-> (process->string (cons cmd args))
      (string-trim-right #\newline)))

(define-syntax run/str
  (syntax-rules ()
    ((_ args)
     (apply run/str1 (quasiquote args)))))

(def (run/lines cmd . args)
   (process->string-list (cons cmd args)))

(def (argv . n)
  (if (pair? n)
      (if (< (car n) (length (command-line)))
          (list-ref (command-line) (car n))
          #f)
      (command-line)))

;; XXX May be Linux-specific!
(def (wexitstatus n)
  (bit-field n 8 16))

(def (run1 cmd . args)
  (let (r (apply system cmd args))
    (display r)
    (if (positive? (car r))
        (wexitstatus (cadr r))
        #f)))

(def (exec-epf pf . redir)
  (def (->string s)
    (cond ((string? s) s)
          ((symbol? s) (symbol->string s))
          (else (error "nyi"))))
  (for-each (fn (r)
                (if (pair? r)
                    (case (car r)
                      ((>) (let (fd (open (->string (cadr r))
                                          (+ open/write open/create open/truncate)))
                             (dup2 fd 1)))
                      ((>>) (let (fd (open (->string (cadr r))
                                           (+ open/write open/append)))
                              (dup2 fd 1)))
                      ((<) (let (fd (open (->string (cadr r))
                                          (+ open/read open/create)))
                             (dup2 fd 0)))
                      (else (error "invalid redir"))
                      )
                    (error "redir not a list")))
            redir)
  (execute (car pf) pf)
  (display "ERROR: couldn't execute: " (current-error-port))
  (write pf (current-error-port))
  (newline (current-error-port))
  (exit 127))

(def (& . epf)
  (let (pid (fork))
    (if (zero? pid)
        (apply exec-epf epf)
        pid)))

(def (wait pid)
  (let (status (waitpid pid 0))
    (wexitstatus (cadr status))))

(def (run! . epf)
  (let (code (wait (apply & epf)))
    (set! *status* code)
    code))

(define (list-set! ls k x)
  (cond ((null? ls) (error "invalid list index"))
        ((zero? k) (set-car! ls x))
        (else (list-set! (cdr ls) (- k 1) x))))

(def *status* 0)

(def *pipestatus* '())

(def (run-pipe! epfs)
  (def (pid? p)
     (and (> p 0)
          (< p 99999999999)))   ; hack
  (when (null? epfs)
    (error "empty pipe not allowed"))
  (let* ((i 0)
         (final (- (length epfs) 1))
         (lp #f)
         (pids (map (fn (epf)
                        (if (< i final)
                            (let (p (pipe))
                              (let (pid (fork))
                                (when (zero? pid)
                                  (dup2 (cadr p) 1)
                                  (when (> i 0)
                                    (dup2 (car lp) 0))
                                  (apply exec-epf epf))
                                (close (cadr p))
                                (when (> i 0)
                                  (close (car lp)))
                                (set! lp p)
                                (set! i (+ 1 i))
                                pid))
                            (let (pid (fork))
                              (when (zero? pid)
                                (dup2 (car lp) 0)
                                (apply exec-epf epf))
                              (close (car lp))
                              (set! i (+ 1 i))
                              pid)))
                    epfs))
         (status (make-list (length epfs) #f))
         )
    (let loop ()
      ;; blocking wait until all processes of the pipe have exited
      ;; reap everything else en passant
      (let (w (waitpid -1 (if (memq #f status)
                              0
                              wait/no-hang)))
        (when (pid? (car w))
          (let ((i (list-index (fn (x) (= x (car w))) pids)))
            (when i
              (list-set! status i (wexitstatus (cadr w))))
            (loop)))))
    (set! *pipestatus* status)
    (set! *status* (last status))
    status
    ))

(define-syntax run
  (syntax-rules ()
    ((_ . epf)
     (let* ((code (wait (apply & (quasiquote epf)))))
       (set! *status* code)
       (set! *pipestatus* (list code))
       code))))

(define-syntax run/pipe
  (syntax-rules ()
    ((_ . epf)
     (run-pipe! (quasiquote epf)))))

(def (~ subject pattern)
  (let (r (fnmatch pattern subject 0))
    (cond ((zero? r) #t)
          ((= r FNM_NOMATCH) #f)
          (else (error "wat")))))

(def (cd dir)
  (let (r (change-directory dir))
    (if r
        #t
        (error (integer->error-string (errno))))))
