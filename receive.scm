; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.

;; original file: big/receive.scm.
(define-module (scsh receive)
  :use-module (scsh alt-syntax))
(export-syntax receive)

(define-syntax receive
  (syntax-rules ()
    ((receive ?vars ?producer . ?body)
     (call-with-values (lambda () ?producer)
       (lambda ?vars . ?body)))))
