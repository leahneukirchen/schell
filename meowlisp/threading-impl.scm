;; Modified for meowlisp by Leah Neukirchen <leah@vuxu.org>
;; Original at https://hg.sr.ht/~bjoli/guile-threading-macros/browse/syntax/threading-impl.scm?rev=a438588baf9d06b0bc47167b2f670b8c651482ec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Linus Björnstam
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These macros work by recursively destructuring the syntax
;; You feed it an exp, a proc and and arguments.
;; The macro then works itself through the args and puts them in
;; ordered. If the macro finds the literal <> or <...> it replaces it with
;; exp and puts it in the ordered list of and runs itself.
;; <> just inserts. <...> makes a list of all arguments and applies it.
(define-syntax exp-insert
  (syntax-rules (<> <...>)
    ;; Execute proc
    ((exp-insert proc (ordered ...))
     (proc ordered ...))
    ;; found <...> which means apply
    ;; fast version
    ((exp-insert exp proc () <...>)
     (apply proc exp))
    ;; slower version
    ((exp-insert exp proc (ordered ...) <...> args ...)
     (apply proc `(ordered ... ,@exp args ...)))
    ;; exp is successfully inserted. Now just collect args.
    ((exp-insert proc (ordered ...) args ...)
     (exp-insert proc (ordered ... args ...)))
    ;; No <> was found. insert exp as the first argument of proc
    ((exp-insert exp proc (ordered ...))
     (exp-insert proc (exp ordered ...)))
    ;; found <>! place exp in it's position in ordered
    ((exp-insert exp proc (ordered ...) <> args ...)
     (exp-insert proc (ordered ... exp) args ...))
    ;; found an arg. proceed!
    ((exp-insert exp proc (ordered ...) arg args ...)
     (exp-insert exp proc (ordered ... arg) args ...))))

(define-syntax exp-insert-right
  (syntax-rules (<> <...>)
    ;; these four just dispatch to exp-insert
    ;; exp is already part of ordered ...
    ;; dispatch to exp-insert for execution
    ((exp-insert-right proc (ordered ...) args ...)
     (exp-insert proc (ordered ... args ...)))
    ;; everything is in ordered ... No <> or <...> was found
    ;; dispatch to exp-insert for execution
    ((exp-insert-right exp proc (ordered ...))
     (exp-insert proc (ordered ... exp)))
    ;; found <...> dispatch the exaxt same form to exp-insert
    ((exp-insert-right exp proc (ordered ...) <...> args ...)
     (exp-insert       exp proc (ordered ...) <...> args ...))
    ;; found <> and insert exp. dispatch to exp-insert for
    ;; execution
    ((exp-insert-right exp proc (ordered ...) <> args ...)
     (exp-insert proc (ordered ... exp) args ...))

    ;; re-iterate exp-insert-right
    ((exp-insert-right exp proc (ordered ...) arg args ...)
     (exp-insert-right exp proc (ordered ... arg) args ...))))


(define-syntax ->
  (syntax-rules ()
    ((-> exp) exp)
    ((-> exp (proc args ...) procs ...)
     (-> (exp-insert exp proc () args ...) procs ...))
    ((-> exp proc procs ...)
     (-> (proc exp) procs ...))))

(define-syntax ->>
  (syntax-rules ()
    ((-> exp) exp)
    ((->> exp (proc args ...) procs ...)
     (->> (exp-insert-right exp proc () args ...) procs ...))
    ((->> exp proc procs ...)
     (->> (proc exp) procs ...))))




(define-syntax and->
  (syntax-rules ()
    ((and-> exp) exp)
    ((and-> exp (proc args ...) procs ...)
     (let ((res (exp-insert exp proc () args ...)))
       (if res
           (and-> res procs ...)
           #f)))
    ((and-> exp proc procs ...)
     (let ((res (proc exp)))
       (if res
           (and-> res procs ...)
           #f)))))

(define-syntax and->>
  (syntax-rules ()
    ((and->> exp) exp)
    ((and->> exp (proc args ...) procs ...)
     (let ((res (exp-insert-right exp proc () args ...)))
       (if res
           (and->> res procs ...)
           #f)))
    ((and-> exp proc procs ...)
     (let ((res (proc exp)))
       (if res
           (and->> res procs ...)
           #f)))))


(define-syntax lambda->
  (syntax-rules ()
    ((lambda-> exps ...)
     (lambda (id) (-> id exps ...)))))

(define-syntax define->
  (syntax-rules ()
    ((define-> id exps ...)
     (define (id arg) (-> arg exps ...)))))

