(define (bitwise-not a) (lognot a))
(define (bitwise-and a b) (logand a b))
(define (bitwise-ior a b) (logior a b))
(define (bitwise-xor a b) (logxor a b))

(define (foreign-source . args) #f)
(defmacro define-foreign args #f)
(defmacro define-errno-syscall args #f)
(defmacro define-record-discloser args #f)
(define ascii->char integer->char)
(define char->ascii char->integer)

;; just pick out the begin forms.
(defmacro define-structure (name interface . body)
  (let loop ((rest body)
	     (result '(begin)))
    (if (null? rest)
	(reverse result)
	(loop (cdr rest)
	      (if (eq? (caar rest) 'begin)
		  (cons (car rest) result)
		  result)))))

;; waitcodes.scm.
(define wait/poll 		WNOHANG)
(define wait/stopped-children	WUNTRACED)

(use-modules (ice-9 slib))
(require 'values)
(require 'format)

(load-from-path "scsh/syntax.scm")
(load-from-path "scsh/receive.scm")
(load-from-path "scsh/let-opt.scm")

;; "delete" primitive is replaced, but doesn't seem worth saving.
(load-from-path "scsh/utilities.scm")
;; replace procedures in utilities.scm with guile primitives.
(set! index string-index)
(set! rindex string-rindex)

(load-from-path "scsh/fname.scm")
(load-from-path "scsh/errno.scm")
(load-from-path "scsh/defrec.scm")
;;(load-from-path "scsh/re.scm")
(load-from-path "scsh/syscalls.scm")
(load-from-path "scsh/time.scm")
(load-from-path "scsh/rw.scm")
(load-from-path "scsh/char-set.scm")
(load-from-path "scsh/rdelim.scm")
(load-from-path "scsh/fr.scm")
(load-from-path "scsh/netconst.scm")
(load-from-path "scsh/network.scm")
(load-from-path "scsh/scsh.scm")
(init-scsh-vars #f)
