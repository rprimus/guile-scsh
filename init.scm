(define (bitwise-not a) (lognot a))
(define (bitwise-and a b) (logand a b))
(define (bitwise-ior a b) (logior a b))
(define (bitwise-xor a b) (logxor a b))

(define-module (guile) :use-module (ice-9 slib))
(require 'macro-by-example)

(load-from-path "scsh/let-opt.scm")

;; "delete" primitive is replaced, but doesn't seem worth saving.
(load-from-path "scsh/utilities.scm")
