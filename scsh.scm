;;; A Scheme shell.
;;; Copyright (c) 1992 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

;;; Guile port is incomplete.

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


