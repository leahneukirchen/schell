; -*- scheme -*-

(define-library (libschell)
  (import
   (except (chibi)
           =
           do
           if
           let)
   (chibi io)
   (chibi string)
   (rename (chibi filesystem)
           (open-pipe pipe)
           (duplicate-file-descriptor dup)
           (duplicate-file-descriptor-to dup2)
           (close-file-descriptor close)
           )
   (rename (chibi process)
           (current-process-id getpid)
           )
   (chibi ast)

   (srfi 1)
   (rename (srfi 98)
           (get-environment-variable getenv))
   (srfi 151)

   (meowlisp)
   (glob)
   )

  (include "libschell/schell.scm")
  
  (export
;  execvp
   pipe
   fork
   exit
   dup2
   dup
   close
   waitpid
   wait/no-hang
   getpid
   cd

   getenv
   $
   run
   run/str
   run/lines
   glob
   argv
   *status* *pipestatus*
   ~

   run/pipe
   &
   wait
   )
  )
