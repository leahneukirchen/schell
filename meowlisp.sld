; -*- scheme -*-

(define-library (meowlisp)
  (import
   (rename (except (chibi) =)
           (if %if)
           (do %do)
           (equal? =)
           (modulo mod)
           )
   (rename (only (scheme division)
                 euclidean-quotient)
           (euclidean-quotient div))
   (chibi show)
   (chibi show pretty)
   (chibi string)
   (srfi 1)
   (srfi 151)
   (only (chibi ast)
         macroexpand)
   )
  (include "meowlisp/threading-impl.scm")
  (include "meowlisp/meowlisp.scm")

  (export
   * + - ... / < <= = => > >= _ abs and append apply assoc begin
   boolean? caar cadr
   car case cdr cdar cddr ceiling
   close-input-port close-output-port
   cond cond-expand
   cons
   current-error-port current-input-port current-output-port
   def
   define-syntax
   display
   do
   dynamic-wind else eq? error
   even?
   expt
   floor
   fn
   for-each
   if
   integer?
   length let list
   map max member min
   negative? newline not null? number->string number?
   odd?
   or output-port? pair? port? positive?
   procedure? quasiquote quote
   real? remainder
   reverse round set! string
   string->number string-append
   string-copy string-fill! string-length
   string-ref string-set!  string<=?  string<?  string=?  string>=?
   string>?  string?  substring symbol->string symbol?
   syntax-rules truncate unless unquote
   unquote-splicing when while
   zero?

   div
   mod

   str
   pp
   ->
   ->>
   macroexpand
   puts
   for
   do
   do1
   with
   concat
   const
   butlast
   chomp
   range
   )
  )
