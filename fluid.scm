;; implementation of scheme48 fluid variables using libguile fluids.

(if (not (defined? 'guile-make-fluid))
    (define guile-make-fluid make-fluid))

(define (make-fluid value)
  (let ((result (guile-make-fluid)))
    (fluid-set! result value)
    result))

(define set-fluid! fluid-set!)

(define fluid fluid-ref)

(define (let-fluid fluid value thunk)
  (with-fluids* (list fluid) (list value) thunk))
