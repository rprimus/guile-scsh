;;; -*-Scheme-*-
;;;
;;; Character Sets package
;;; - ported from MIT Scheme runtime
;;;   by Brian D. Carlstrom
;;; - Rehacked & extended by Olin Shivers 6/98.

;;; This is not great code. Char sets are represented as 256-char
;;; strings. If char i is ASCII 0, then it isn't in the set; if char i
;;; is ASCII 1, then it is in the set.
;;; - Should be rewritten to use bit strings, or at least byte vecs.
;;; - Is ASCII/Latin-1 specific. Would certainly have to be rewritten
;;;   for Unicode.
;;; - The standard character sets are not Latin-1 compliant, just ASCII.

;;; This code uses jar's DEFINE-RECORD-TYPE macro to define the char-set
;;; record type, because the scsh-standard DEFINE-RECORD form automatically
;;; defines a COPY-FOO function, which is not the one we want, being a shallow
;;; copy of the record fields.

;;; New dfns:
;;; (char-set=  cs1 cs2 ...)
;;; (char-set<= cs1 cs2 ...)
;;; (char-set-fold kons knil cs)
;;; (char-set-for-each f cs)
;;; (char-set-copy cs)
;;; (char-set-size cs)
;;; char-set:printing		(char-printing?  c)
;;; char-set:blank		(char-blank?     c)   
;;; char-set:control		(char-control?   c) 
;;; char-set:hex-digit		(char-hex-digit? c)
;;; char-set:ascii		(char-ascii?     c)   
;;; char-set:empty
;;; char-set:full
;;; char-set-every? pred cs
;;; char-set-any    pred cs
;;; char-set-adjoin  cset char -> cset
;;; char-set-adjoin! cset char -> cset
;;; char-set-delete  cset char -> cset
;;; char-set-delete! cset char -> cset

(define-module (scsh char-set)
  :use-module (scsh ascii)
  :use-module (scsh utilities)
  :use-module (scsh jar-defrecord))

(export char:newline char:tab char:page char:return char:space char:vtab
	char-ascii?

	char-set?
	char-set-copy
	char-set=
	char-set<=
	char-set-size

	char-set-adjoin  char-set-delete
	char-set-adjoin! char-set-delete!
	char-set-for-each
	char-set-fold reduce-char-set

	char-set
	chars->char-set
	string->char-set
	ascii-range->char-set
	predicate->char-set
	->char-set

	char-set-members
	char-set-contains?

	char-set-every?
	char-set-any

	char-set-invert
	char-set-union
	char-set-intersection
	char-set-difference

	char-set-invert!
	char-set-union!
	char-set-intersection!
	char-set-difference!

	char-set:lower-case
	char-set:upper-case
	char-set:alphabetic
	char-set:numeric
	char-set:alphanumeric
	char-set:graphic
	char-set:printing
	char-set:whitespace
	char-set:blank
	char-set:control
	char-set:punctuation
	char-set:hex-digit
	char-set:ascii
	char-set:empty
	char-set:full

	;; This is not properly part of the interface,
	;; and should be moved to an internals interface --
	;; it is used by rdelim.scm code.
	char-set:s
	)

(define char:newline (ascii->char 13))
(define char:tab     (ascii->char  9))
(define char:vtab    (ascii->char 11))
(define char:page    (ascii->char 12))
(define char:return  (ascii->char 10))
(define char:space   (ascii->char 32))

(define (string-copy s) (substring s 0 (string-length s)))

(define (string-fill-range! str lower upper ch)
  (do ((index lower (+ index 1)))
    ((>= index upper) str)
    (string-set! str index ch)))

(define (char-ascii? char)
  (let ((maybe-ascii (char->ascii char)))
    (and (<= 0 maybe-ascii 127) maybe-ascii)))

;;;; Character Sets

;(define-record char-set
;  s)	; 256-char string; each char is either ASCII 0 or ASCII 1.

;;; Use jar's record macro.
(define-record-type char-set :char-set
  (make-char-set s)
  char-set?
  (s char-set:s))

(define (char-set-copy cs) (make-char-set (string-copy (char-set:s cs))))

;;; The = and <= code is ugly because it's n-ary.

(define (char-set= cs1 . rest)
  (let ((s1 (char-set:s cs1)))
    (every (lambda (cs) (string=? s1 (char-set:s cs)))
	   rest)))

(define (char-set<= cs1 . rest)
  (let lp ((s1 (char-set:s cs1))
	   (rest rest))
    (or (not (pair? rest))
	(let ((s2 (char-set:s (car rest)))
	      (rest (cdr rest)))
	  (let lp2 ((i 255))
	    (if (< i 0) (lp s2 rest)
		(and (<= (char->ascii (string-ref s1 i))
			 (char->ascii (string-ref s2 i)))
		     (lp2 (- i 1)))))))))


(define (char-set-size cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255) (size 0))
      (if (< i 0) size
	  (lp (- i 1)
	      (if (= 0 (char->ascii (string-ref s i))) size (+ size 1)))))))

(define (set-char-set cs in? . chars)
  (let ((s (string-copy (char-set:s cs)))
	(val (if in? (ascii->char 1) (ascii->char 0))))
    (for-each (lambda (c) (string-set! s (char->ascii c) val))
	      chars)
    (make-char-set s)))

(define (set-char-set! cs in? . chars)
  (let ((s (char-set:s cs))
	(val (if in? (ascii->char 1) (ascii->char 0))))
    (for-each (lambda (c) (string-set! s (char->ascii c) val))
	      chars))
  cs)

(define (char-set-adjoin  cs . chars) (apply set-char-set  cs #t chars))
(define (char-set-adjoin! cs . chars) (apply set-char-set! cs #t chars))
(define (char-set-delete  cs . chars) (apply set-char-set  cs #f chars))
(define (char-set-delete! cs . chars) (apply set-char-set! cs #f chars))

(define (char-set-for-each proc cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255))
      (cond ((>= i 0)
	     (if (not (= 0 (char->ascii (string-ref s i))))
		 (proc (ascii->char i)))
	     (lp (- i 1)))))))

(define (char-set-fold kons knil cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255) (ans knil))
      (if (< i 0) ans
	  (lp (- i 1)
	      (if (= 0 (char->ascii (string-ref s i)))
		  ans
		  (kons (ascii->char i) ans)))))))

(define reduce-char-set (deprecated-proc char-set-fold 'char-set-fold
					 "Use char-set-fold instead."))

(define (char-set-every? pred cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255))
      (or (< i 0)
	  (if (= 0 (char->ascii (string-ref s i)))
	      (lp (- i 1))
	      (and (pred (ascii->char i))
		   (lp (- i 1))))))))

(define (char-set-any pred cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255))
      (and (>= i 0)
	   (if (= 0 (char->ascii (string-ref s i)))
	       (lp (- i 1))
	       (or (pred (ascii->char i))
		   (lp (- i 1))))))))


(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (let ((s (make-string 256 (ascii->char 0))))
    (for-each (lambda (char) 
		(string-set! s (char->ascii char) (ascii->char 1)))
	      chars)
    (make-char-set s)))

(define (string->char-set str)
  (let ((s (make-string 256 (ascii->char 0))))
    (do ((i (- (string-length str) 1) (- i 1)))
	((< i 0) (make-char-set s))
      (string-set! s (char->ascii (string-ref str i))
		   (ascii->char 1)))))

(define (ascii-range->char-set lower upper)
  (let ((s (make-string 256 (ascii->char 0))))
    (string-fill-range! s lower upper (ascii->char 1))
    (make-char-set s)))

(define (predicate->char-set predicate)
  (let ((s (make-string 256)))
    (let lp ((i 255))
      (if (>= i 0)
	  (begin (string-set! s i (if (predicate (ascii->char i)) 
				      (ascii->char 1)
				      (ascii->char 0)))
		 (lp (- i 1)))))
    (make-char-set s)))


;;; {string, char, char-set, char predicate} -> char-set

(define (->char-set x)
  (cond ((char-set? x) x)
	((string? x) (string->char-set x))
	((char? x) (char-set x))
	((procedure? x) (predicate->char-set x))
	(else (error "->char-set: Not a charset, string, char, or predicate."
		     x))))


;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (char-set-members cs)
  (let ((s (char-set:s cs)))
    (let lp ((i 255) (ans '()))
      (if (< i 0) ans
	  (lp (- i 1)
	      (if (zero? (char->ascii (string-ref s i))) ans
		  (cons (ascii->char i) ans)))))))

;;; De-releasing CHAR-SET-MEMBER?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; No other way to do it. MIT Scheme defines it (c-s-m? cset char); scsh 0.3
;;; defined it (c-s-m? char cset). MIT Scheme's arg order is not consistent 
;;; with the MEMBER? procedure or common math notation, but they were here
;;; first, so I didn't want to just silently invert their arg order -- could
;;; break code. I ended up just choosing a new proc name that consistent with
;;; its arg order -- (CHAR-SET-CONTAINS? cset char).

(define (char-set-contains? cs char)
  (not (zero? (char->ascii (string-ref (char-set:s cs)
				       (char->ascii char))))))

;;; This actually isn't exported. Just CYA.
(define (char-set-member? . args)
  (error "CHAR-SET-MEMBER? is no longer provided. Use CHAR-SET-CONTAINS? instead."))


;;; Set algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-set-invert cs)
  (predicate->char-set (lambda (char)
			 (not (char-set-contains? cs char)))))

(define (char-set-union . csets)
  (if (pair? csets)
      (apply char-set-union! (char-set-copy (car csets)) (cdr csets))
      char-set:empty))

(define (char-set-intersection . csets)
  (if (pair? csets)
      (apply char-set-intersection! (char-set-copy (car csets)) (cdr csets))
      char-set:full))

(define (char-set-difference cs1 . csets)
  (if (pair? csets)
      (apply char-set-difference! (char-set-copy cs1) csets)
      cs1))


;;; Linear set-algebraic ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These guys are allowed, but not required, to side-effect their first
;;; argument when computing their result. In other words, you must use them
;;; as if they were completely functional, just like their non-! counterparts,
;;; and you must additionally ensure that their first arguments are "dead"
;;; at the point of call. In return, we promise a more efficient result, plus
;;; allowing you to always assume char-sets are unchangeable values.

;;; Apply P to each index and it's char in S: (P I C).
;;; Used by the intersection & difference.

(define (string-iter p s)
  (let lp ((i (- (string-length s) 1)))
    (cond ((>= i 0)
	   (p i (string-ref s i))
	   (lp (- i 1))))))

(define (char-set-invert! cset)
  (let ((s (char-set:s cset)))
    (string-iter (lambda (i c)
		     (string-set! s i (ascii->char (- 1 (char->ascii c)))))
		 s))
  cset)

(define (char-set-union! cset1 . csets)
  (let ((s (char-set:s cset1)))
    (for-each (lambda (cset)
		(char-set-for-each (lambda (c)
				     (string-set! s (char->ascii c)
						  (ascii->char 1)))
				   cset))
	      csets))
  cset1)

(define (char-set-intersection! cset1 . csets)
  (let ((s (char-set:s cset1)))
    (for-each (lambda (cset)
		(string-iter (lambda (i c)
			       (if (zero? (char->ascii c))
				   (string-set! s i (ascii->char 0))))
			     (char-set:s cset)))
	      csets))
  cset1)

(define (char-set-difference! cset1 . csets)
  (let ((s (char-set:s cset1)))
    (for-each (lambda (cset)
		(char-set-for-each (lambda (c)
					  (string-set! s (char->ascii c)
						       (ascii->char 0)))
				   cset))
	      csets))
  cset1)



;;;; System Character Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char-set:lower-case (ascii-range->char-set #x61 #x7B))
(define char-set:upper-case (ascii-range->char-set #x41 #x5B))
(define char-set:alphabetic
  (char-set-union char-set:upper-case char-set:lower-case))
(define char-set:numeric (ascii-range->char-set #x30 #x3A))
(define char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))
(define char-set:graphic  (ascii-range->char-set #x21 #x7F))
(define char-set:printing (ascii-range->char-set #x20 #x7F))
(define char-set:whitespace (char-set char:tab  char:newline char:vtab
				      char:page char:return  char:space))
(define char-set:blank (char-set char:space char:tab))
(define char-set:control (char-set-union (ascii-range->char-set 0 32)
					 (char-set (ascii->char 127))))
(define char-set:punctuation
  (string->char-set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))
(define char-set:ascii (ascii-range->char-set 0 128))
(define char-set:empty (char-set))
(define char-set:full (char-set-invert char-set:empty))
