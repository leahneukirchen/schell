# Schell, a lispy shell scripting language

Schell is an experimental Scheme-based language for shell scripting.
The language is currently roughly as featureful as the Plan9 rc shell.

Schell uses a dialect of Scheme called meowlisp.

WARNING: This code is very alpha, be careful using it!

## Quick example

```
(cd "/tmp")
(with (tmp (run/str (mktemp -p ".")))
  (run (:pipe (date)
              (:epf (rev) (> ,tmp))))
  (puts (str "The time backwards is: " (run/str (cat ,tmp))))
  (run (rm ,tmp)))
```

## meowlisp

meowlisp differs from Scheme in some significant ways:

* `def` is used instead of `define`.
  It is used for definitions of variables and functions:
  
  ```
  (def meaning-of-life 42)
  
  (def (square x)
    (* x x))
  ```

* `fn` is used instead of `lambda`.

* `with` is like `let*` with fewer parentheses:

  ```
  (with (a 6 b 7)
    (* a b))
  ```

* `if` is multi-legged:

  ```
  (if condition
      then-branch
    else-branch
    else-branch
    else-branch)
  ```

* `when` and `unless` can be used instead of `if` with a single branch.

* `do` is used instead of `begin`.

* `do1` is a variant of `do` that evaluates the expressions
  in order and then returns the value of the first expression.
  
* Iteration over lists is possible with `for`:

  ```
  (for x '(foo bar baz)
    (puts x))
  ```
  
* Clojure-style threading macros `->` and `->>` are provided.

meowlisp provides the following utility functions:

* `str` returns a concatenated string representation of all arguments.

* `concat` concatenates all arguments, which are lists.

* `pp` pretty-prints all arguments.

* `puts` prints all arguments separated by newlines, unless they
  contain newlines already.
  
* `(const x)` returns a function that takes any arguments and always returns `x`.

* `butlast` returns a list except for the last element.

## libschell

Libschell provides the basic primitives of a shell:

* `($ ENV)` returns the value of `$ENV`.

* `(~ subject pattern)` matches the string subject against the shell
  wildcard pattern.
  
* `(argv N)` returns the N-th argument or #f if there are fewer.

* `(argv)` returns a list of all arguments.

* `(glob pattern)` returns a list of files that match the pattern.

* `*status*` contains the exit status of the last command invoked.

* `*pipestatus*` contains the list of exit statuses of commands
  invoked in a pipe.

* `(run ...)`

* `(run/str ...)`

* `(& ...)`

## Schell

Schell bundles together meowlisp and libschell as a chibi-scheme
language and provides a wrapper script for convenient execution.

Schell scripts should start with `#!/usr/bin/env schell`.

## Installation

TBD.  Run `make` and symlink `schell` into your PATH for now.

## Copyright

Copyright (C) 2021 Leah Neukirchen <leah@vuxu.org>

Licensed under the terms of the MIT license, see LICENSE.
