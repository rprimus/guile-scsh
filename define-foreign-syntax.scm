(define-module (scsh define-foreign-syntax))
(export-syntax foreign-source define-foreign)

;; used in network redelim rx/re-low, sighandlers, syscalls, time.
(define (foreign-source . args) #f)
(defmacro define-foreign args #f)
