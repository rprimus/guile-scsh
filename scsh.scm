;;; A Scheme shell.
;;; Copyright (c) 1992 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

;;; Guile port is incomplete.

;;; Call THUNK, then die.
;;; A clever definition in a clever implementation allows the caller's stack
;;; and dynamic env to be gc'd away, since this procedure never returns.

;;; (define (call-terminally thunk)
;;;  (with-continuation #f (lambda () (thunk) (exit 0))))
;;;  ;; Alternatively: (with-continuation #f thunk)

;;; More portably, but less usefully:
(define (call-terminally thunk)
  (thunk)
  (exit 0))

;;; Environment stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These two functions are obsoleted by the more general INFIX-SPLITTER and
;;; JOIN-STRINGS functions. However, we keep SPLIT-COLON-LIST defined
;;; internally so the top-level startup code (INIT-SCSH) can use it
;;; to split up $PATH without requiring the field-splitter or regexp code.

(define (split-colon-list clist)
  (let ((len (string-length clist)))
    (if (= 0 len) '()			; Special case "" -> ().

	;; Main loop.
	(let split ((i 0))
	  (cond ((index clist #\: i) =>
		 (lambda (colon)
		   (cons (substring clist i colon)
			 (split (+ colon 1)))))
		(else (list (substring clist i len))))))))

;;; Unix colon lists typically use colons as separators, which
;;; is not as clean to deal with as terminators, but that's Unix.
;;; Note ambiguity: (s-l->c-l '()) = (s-l->c-l '("")) = "".

; (define (string-list->colon-list slist)
;   (if (pair? slist)
;       (apply string-append
; 	     (let colonise ((lis slist))	; LIS is always
; 	       (let ((tail (cdr lis))) 		; a pair.
; 		 (cons (car lis)
; 		       (if (pair? tail)
; 			   (cons ":" (colonise tail))
; 			   '())))))
;       ""))	; () case.


(define (alist-delete key alist)
  (filter (lambda (key/val) (not (equal? key (car key/val)))) alist))

(define (alist-update key val alist)
  (cons (cons key val)
	(alist-delete key alist)))

;;; Remove shadowed entries from ALIST. Preserves element order.
;;; (This version shares no structure.)

(define (alist-compress alist) 
  (reverse (let compress ((alist alist) (ans '()))
	     (if (pair? alist)
		 (let ((key/val (car alist))
		       (alist (cdr alist)))
		   (compress alist (if (assoc (car key/val) ans) ans
				       (cons key/val ans))))
		 ans))))

;; Tail-recursive loops suck.
;; (define (alist-compress alist)
;;   (loop (initial (ans '()))
;;	   (for key/val in alist)
;;   
;;	   (when (not (assoc (car key/val) ans)))
;;	   (next (ans (cons key/val ans)))
;;   
;;	   (result (reverse ans))))

(define (add-before elt before list)
  (let rec ((list list))
    (if (pair? list)
	(let ((x (car list)))
	  (if (equal? x before)
	      (cons elt list)
	      (cons x (rec (cdr list)))))
	(cons elt list))))

;;; In ADD-AFTER, the labelled LET adds ELT after the last occurrence of AFTER
;;; in LIST, and returns the list. However, if the LET finds no occurrence 
;;; of AFTER in LIST, it returns #F instead.

(define (add-after elt after list)
  (or (let rec ((list list))
	(if (pair? list)
	    (let* ((x (car list))
		   (tail (cdr list))
		   (ans (rec tail))) ; #f if AFTER wasn't encountered.
	      (cond (ans (cons x ans))
		    ((equal? x after)
		     (cons x (cons elt tail)))
		    (else #f)))		; AFTER doesn't appear in LIST.
	    #f))			; AFTER doesn't appear in LIST.
      (cons elt list))) 

;;; Or, just say...
;;; (reverse (add-before elt after (reverse list)))

(define (with-env* alist-delta thunk)
  (let* ((old-env #f)
	 (new-env (reduce (lambda (alist key/val)
			    (alist-update (car key/val) (cdr key/val) alist))
			  (env->alist)
			  alist-delta)))
    (dynamic-wind
      (lambda ()
	(set! old-env (env->alist))
	(alist->env new-env))
      thunk
      (lambda ()
	(set! new-env (env->alist))
	(alist->env old-env)))))

(define (with-total-env* alist thunk)
  (let ((old-env (env->alist)))
    (dynamic-wind
      (lambda ()
	(set! old-env (env->alist))
	(alist->env alist))
      thunk
      (lambda ()
	(set! alist (env->alist))
	(alist->env old-env)))))


(define (with-cwd* dir thunk)
  (let ((old-wd #f))
    (dynamic-wind
      (lambda ()
	(set! old-wd (cwd))
	(chdir dir))
      thunk
      (lambda ()
	(set! dir (cwd))
	(chdir old-wd)))))

(define (with-umask* mask thunk)
  (let ((old-mask #f))
    (dynamic-wind
      (lambda ()
	(set! old-mask (umask))
	(set-umask mask))
      thunk
      (lambda ()
	(set! mask (umask))
	(set-umask old-mask)))))

;;; Sugar:

(define-simple-syntax (with-cwd dir . body)
  (with-cwd* dir (lambda () . body)))

(define-simple-syntax (with-umask mask . body)
  (with-umask* mask (lambda () . body)))

(define-simple-syntax (with-env delta . body)
  (with-env* `delta (lambda () . body)))

(define-simple-syntax (with-total-env env . body)
  (with-total-env* `env (lambda () . body)))


;;; Stdio/stdport sync procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stdio->stdports)
  (set-current-input-port!  (fdes->inport 0))
  (set-current-output-port! (fdes->outport 1))
  (set-error-output-port!   (fdes->outport 2)))

(define (with-stdio-ports* thunk)
  (with-current-input-port (fdes->inport 0)
    (with-current-output-port (fdes->outport 1)
      (with-error-output-port (fdes->outport 2)
	(thunk)))))

(define-simple-syntax (with-stdio-ports body ...)
  (with-stdio-ports* (lambda () body ...)))


(define (stdports->stdio)
  (dup (current-input-port)  0)
  (dup (current-output-port) 1)
  (dup (error-output-port)   2))

(define (exit . maybe-status)
  (flush-all-ports)
  (primitive-exit (:optional  maybe-status 0))
  (display "The evil undead walk the earth." 2)
  (error "(exit) returned."))


;;; The classic T 2.0 primitive.
;;; This definition works for procedures running on top of Unix systems.
(define (halts? proc) #t)


;;; Low-level init absolutely required for any scsh program.

;;(define (init-scsh-hindbrain relink-ff?)
;;  (if relink-ff? (lookup-all-externals)) ; Re-link C calls.
;;  (init-fdports!)
;;  (%install-unix-scsh-handlers))


;;; Some globals:
(define home-directory "")
(define exec-path-list '())

(define (init-scsh-vars quietly?)
  (set! home-directory
	(cond ((getenv "HOME") => ensure-file-name-is-nondirectory)
	      (else (if (not quietly?)
			(warn "Starting up with no home directory ($HOME)."))
		    "/")))
  (set! exec-path-list
	(cond ((getenv "PATH") => split-colon-list)
	      (else (if (not quietly?)
			(warn "Starting up with no path ($PATH)."))
		    '()))))
