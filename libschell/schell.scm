(macroexpand '(with (a 5 b 3) (pp a b)))

(define-syntax $
  (syntax-rules ()
    ((_ name)
     (getenv (str (quote name))))))

(def *status* 0)

(def *pipestatus* '())

(def (set-status! status)
  (if (pair? status)
      (do
        (set! *pipestatus* status)
        (set! *status* (last status)))
    (set! *pipestatus* (list status))
    (set! *status* status))
  status)

(def (run/str1 epf)
  (with (p (pipe)
         pid (fork))
    (if (zero? pid)
        (do
            (close (car p))
            (dup2 (cadr p) 1)
            (close (cadr p))
            (apply exec-epf epf))
        (close (cadr p))
;??     (set-non-blocking! (car p))
        (do1
          (-> (car p)
              open-input-file-descriptor
              port->string
              chomp)
          (set-status! (wait pid))))))

(define-syntax run/str
  (syntax-rules ()
    ((_ . args)
     (run/str1 (quasiquote args)))))

(def (run/lines1 epf)
   (-> (run/str1 epf)
       (string-split #\newline)))

(define-syntax run/lines
  (syntax-rules ()
    ((_ . args)
     (run/lines1 (quasiquote args)))))

(def (argv . n)
  (if (pair? n)
      (if (< (car n) (length (command-line)))
          (list-ref (command-line) (car n))
          #f)
      (command-line)))

;; XXX May be Linux-specific!
(def (wexitstatus n)
  (bit-field n 8 16))

(def (exec-epf pf . redir)
  (def (->string s)
    (cond ((string? s) s)
          ((symbol? s) (symbol->string s))
          (else (error "nyi"))))
  (for-each (fn (r)
                (if (pair? r)
                    (case (car r)
                      ((>) (with (fd (open (->string (cadr r))
                                          (+ open/write open/create open/truncate)))
                             (dup2 fd 1)))
                      ((>>) (with (fd (open (->string (cadr r))
                                           (+ open/write open/append)))
                              (dup2 fd 1)))
                      ((<) (with (fd (open (->string (cadr r))
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
  (with (pid (fork))
    (if (zero? pid)
        (apply exec-epf epf)
        pid)))

(def (wait pid)
  (with (status (waitpid pid 0))
    (wexitstatus (cadr status))))

(define (list-set! ls k x)
  (cond ((null? ls) (error "invalid list index"))
        ((zero? k) (set-car! ls x))
        (else (list-set! (cdr ls) (- k 1) x))))

(def (exec-epf2 pf)
  (if (= :epf (car pf))
      (apply exec-epf (cdr pf))
    (exec-epf pf)))

(def (run-pipe! epfs)
  (def (pid? p)
     (and (> p 0)
          (< p 99999999999)))   ; hack
  (when (null? epfs)
    (error "empty pipe not allowed"))
  (if (null? (cdr epfs))
    (run! (car epfs))
    (with (i 0
           final (- (length epfs) 1)
           lp #f
           pids (map (fn (epf)
                         (if (< i final)
                             (with (p (pipe))
                               (with (pid (fork))
                                 (when (zero? pid)
                                   (dup2 (cadr p) 1)
                                   (when (> i 0)
                                     (dup2 (car lp) 0))
                                   (exec-epf2 epf))
                                 (close (cadr p))
                                 (when (> i 0)
                                   (close (car lp)))
                                 (set! lp p)
                                 (set! i (+ 1 i))
                                 pid))
                             (with (pid (fork))
                               (when (zero? pid)
                                 (dup2 (car lp) 0)
                                 (exec-epf2 epf))
                               (close (car lp))
                               (set! i (+ 1 i))
                               pid)))
                     epfs)
           status (make-list (length epfs) #f)
           )
      (let loop ()
        ;; blocking wait until all processes of the pipe have exited
        ;; reap everything else en passant
        (with (w (waitpid -1 (if (memq #f status)
                                0
                                wait/no-hang)))
          (when (pid? (car w))
            (with (i (list-index (fn (x) (= x (car w))) pids))
              (when i
                (list-set! status i (wexitstatus (cadr w))))
              (loop)))))
      (set-status! status)
      )))

(def :pipe ':pipe)
(def :epf ':epf)

(def (run! pf)
  (pp pf)
  (cond ((= (car pf) :pipe)
         (run-pipe! (cdr pf)))

        ((= (car pf) '!)
         (run! (cadr pf))
         (set-status! (if (zero? *status*) 1 0)))

        ((= (car pf) '&&)
         (set-status! 0)
         (let loop ((pfs (cdr pf)))
           (when (pair? pfs)
             (run! (car pfs))
             (when (zero? *status*)
               (loop (cdr pfs))))))

        ((= (car pf) '||)
         (set-status! 1)
         (let loop ((pfs (cdr pf)))
           (when (pair? pfs)
             (run! (car pfs))
             (unless (zero? *status*)
               (loop (cdr pfs))))))

        ((= (car pf) :epf)
         (set-status! (wait (apply & (cdr pf)))))

        (else
         (set-status! (wait (& pf))))))

(define-syntax run
  (syntax-rules ()
    ((_ pf)
     (run! (quasiquote pf)))
    ((_ . epf)
     (run! (cons :epf (quasiquote epf))))))

(define-syntax run/pipe
  (syntax-rules ()
    ((_ . epf)
     (run-pipe! (quasiquote epf)))))

(def (~ subject pattern)
  (with (r (fnmatch pattern subject 0))
    (cond ((zero? r) #t)
          ((= r FNM_NOMATCH) #f)
          (else (error "wat")))))

(def (cd dir)
  (with (r (change-directory dir))
    (if r
        #t
        (error (integer->error-string (errno))))))
