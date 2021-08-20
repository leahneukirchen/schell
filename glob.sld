;; -*- scheme -*-

(define-library (glob)
  (import (scheme base))
  (include-shared "glob/glob")
  (include "glob/glob.scm")
  (export
    glob
    fnmatch
    FNM_NOMATCH
    ))
