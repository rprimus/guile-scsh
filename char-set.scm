;;; -*-Scheme-*-
;;;
;;; Character Sets package
;;; ported from MIT Scheme runtime
;;; by Brian D. Carlstrom
;;; Sleazy code.

(define char:newline (ascii->char 13))
(define char:tab (ascii->char 9))
(define char:linefeed (ascii->char 13))
(define char:page (ascii->char 12))
(define char:return (ascii->char 10))
(define char:space (ascii->char 32))

(define (string-fill-range! str lower upper ch)
  (do ((index lower (+ index 1)))
    ((>= index upper) str)
    (string-set! str index ch)))

(define (char-ascii? char)
  (let ((maybe-ascii (char->ascii char)))
    (and (<= 0 maybe-ascii 127) maybe-ascii)))

;;;; Character Sets

(define (char-set? object)
  (and (string? object)
       (= (string-length object) 256)))

(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (let ((char-set (make-string 256 (ascii->char 0))))
    (for-each (lambda (char) 
		(string-set! char-set (char->ascii char) (ascii->char 1)))
	      chars)
    char-set))

(define (string->char-set str)
  (let ((char-set (make-string 256 (ascii->char 0))))
    (do ((i (- (string-length str) 1) (- i 1)))
	((< i 0) char-set)
      (string-set! char-set (char->ascii (string-ref str i))
		   (ascii->char 1)))))

(define (ascii-range->char-set lower upper)
  (let ((char-set (make-string 256 (ascii->char 0))))
    (string-fill-range! char-set lower upper (ascii->char 1))
    char-set))

(define (predicate->char-set predicate)
  (let ((char-set (make-string 256)))
    (let loop ((code 0))
      (if (< code 256)
	  (begin (string-set! char-set code
				 (if (predicate (ascii->char code)) 
				     (ascii->char 1)
				     (ascii->char 0)))
		 (loop (+ 1 code)))))
    char-set))


;;; {string, char, char-set, char predicate} -> char-set

(define (->char-set x)
  (cond ((char-set? x) x)
	((string? x) (string->char-set x))
	((char? x) (char-set x))
	((procedure? x) (predicate->char-set x))
	(else (error "->char-set: Not a charset, string, char, or predicate."
		     x))))


;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (char-set-members char-set)
  (define (loop code)
    (cond ((>= code 256) '())
	  ((zero? (char->ascii (string-ref char-set code))) (loop (+ 1 code)))
	  (else (cons (ascii->char code) (loop (+ 1 code))))))
  (loop 0))

;;; De-releasing CHAR-SET-MEMBER?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; No other way to do it. MIT Scheme defines it (c-s-m? cset char); scsh 0.3
;;; defined it (c-s-m? char cset). MIT Scheme's arg order is not consistent 
;;; with the MEMBER? procedure or common math notation, but they were here
;;; first, so I didn't want to just silently invert their arg order -- could
;;; break code. I ended up just choosing a new proc name that consistent with
;;; its arg order -- (CHAR-SET-CONTAINS? cset char).

(define (char-set-contains? char-set char)
  (not (zero? (char->ascii (string-ref char-set (char->ascii char))))))

;;; This actually isn't exported. Just CYA.
(define (char-set-member? . args)
  (error "CHAR-SET-MEMBER? is no longer provided. Use CHAR-SET-CONTAINS? instead."))

(define (char-set-invert char-set)
  (predicate->char-set
   (lambda (char) (not (char-set-contains? char-set char)))))

(define (char-set-union char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (or (char-set-contains? char-set-1 char)
	 (char-set-contains? char-set-2 char)))))

(define (char-set-intersection char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-contains? char-set-1 char)
	  (char-set-contains? char-set-2 char)))))

(define (char-set-difference char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-contains? char-set-1 char)
	  (not (char-set-contains? char-set-2 char))))))

;;;; System Character Sets

(define char-set:upper-case (ascii-range->char-set #x41 #x5B))
(define char-set:lower-case (ascii-range->char-set #x61 #x7B))
(define char-set:numeric (ascii-range->char-set #x30 #x3A))
(define char-set:graphic (ascii-range->char-set #x20 #x7F))
(define char-set:not-graphic (char-set-invert char-set:graphic))
(define char-set:whitespace
  (char-set char:newline char:tab char:linefeed 
	    char:page char:return char:space))
(define char-set:not-whitespace (char-set-invert char-set:whitespace))
(define char-set:alphabetic
  (char-set-union char-set:upper-case char-set:lower-case))
(define char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))
(define char-set:standard
  (char-set-union char-set:graphic (char-set char:newline)))

(define (char-upper-case? char)
  (char-set-contains? char-set:upper-case char))

(define (char-lower-case? char)
  (char-set-contains? char-set:lower-case char))

(define (char-numeric? char)
  (char-set-contains? char-set:numeric char))

(define (char-graphic? char)
  (char-set-contains? char-set:graphic char))

(define (char-whitespace? char)
  (char-set-contains? char-set:whitespace char))

(define (char-alphabetic? char)
  (char-set-contains? char-set:alphabetic char))

(define (char-alphanumeric? char)
  (char-set-contains? char-set:alphanumeric char))

(define (char-standard? char)
  (char-set-contains? char-set:standard char))

;;; Bullshit legalese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;$Header: /sources/guile/guile/guile-scsh/Attic/char-set.scm,v 1.1 1997/01/25 18:26:38 ghouston Exp $

;Copyright (c) 1988 Massachusetts Institute of Technology

;This material was developed by the Scheme project at the Massachusetts
;Institute of Technology, Department of Electrical Engineering and
;Computer Science.  Permission to copy this software, to redistribute
;it, and to use it for any purpose is granted, subject to the following
;restrictions and understandings.

;1. Any copy made of this software must include this copyright notice
;in full.

;2. Users of this software agree to make their best efforts (a) to
;return to the MIT Scheme project any improvements or extensions that
;they make, so that these may be included in future releases; and (b)
;to inform MIT of noteworthy uses of this software.

;3. All materials developed as a consequence of the use of this
;software shall duly acknowledge such use, in accordance with the usual
;standards of acknowledging credit in academic research.

;4. MIT has made no warrantee or representation that the operation of
;this software will be error-free, and MIT is under no obligation to
;provide any services, by way of maintenance, update, or otherwise.

;5. In conjunction with products arising from the use of this material,
;there shall be no use of the name of the Massachusetts Institute of
;Technology nor of any adaptation thereof in any advertising,
;promotional, or sales literature without prior written consent from
;MIT in each case. 

