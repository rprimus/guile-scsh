;;; Scheme Underground string-processing library		-*- Scheme -*-
;;; Olin Shivers 11/98

;;; SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT
;;; This is *draft* code for a SRFI proposal. If you see this notice in 
;;; production code, you've got obsolete, bad source -- go find the final 
;;; non-draft code on the Net.
;;; SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT -- SRFI DRAFT

;;; Some of this code had (extremely distant) origins in MIT Scheme's string
;;; lib, and was substantially reworked by Olin Shivers (shivers@ai.mit.edu)
;;; 9/98. As such, it is
;;;     Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; The copyright terms are essentially open-software terms;
;;; the precise terms are at the end of this file.
;;; 
;;; The KMP string-search code was massively rehacked from Stephen Bevan's
;;; code, written for scmlib, and is thus covered by the GPL. If that's a
;;; problem, write one from scratch (there are citations to standard textbooks
;;; in the comments), or rip it out and use the ten-line doubly-nested loop
;;; that's commented out just above this code.
;;;
;;; I wish I could mark definitions in this code to be inlined.
;;; Certain functions could benefit from compiler support.
;;; 
;;; My policy on checking start/end substring specs is not uniform.
;;; I avoided doing arg checks when the function directly calls another
;;; lower-level function that will check the start/end specs as well.
;;; This has the advantage of not doing redundant checks, but the disadvantage
;;; is that errors are not reported early, at the highest possible call.
;;; There's not much high-level error checking of the other args, anyway.
;;;     -Olin

;;; Exports:
;;; string-map    string-map!
;;; string-fold       string-unfold
;;; string-fold-right string-unfold-right 
;;; string-tabulate
;;; string-for-each string-iter
;;; string-every string-any
;;; string-compare string-compare-ci
;;; substring-compare substring-compare-ci
;;; string= string< string> string<= string>= string<>
;;; string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
;;; substring=     substring<>		substring-ci=  substring-ci<>
;;; substring<     substring>		substring-ci<  substring-ci>
;;; substring<=    substring>=		substring-ci<= substring-ci>=
;;; string-upper-case? string-lower-case?
;;; capitalize-string  capitalize-words  string-downcase  string-upcase
;;; capitalize-string! capitalize-words! string-downcase! string-upcase!
;;; string-take string-take-right
;;; string-drop string-drop-right
;;; string-pad string-pad-right
;;; string-trim string-trim-right string-trim-both
;;; string-filter string-delete
;;; string-index string-index-right string-skip string-skip-right
;;; string-prefix-count string-prefix-count-ci
;;; string-suffix-count string-suffix-count-ci
;;; substring-prefix-count substring-prefix-count-ci
;;; substring-suffix-count substring-suffix-count-ci
;;; string-prefix? string-prefix-ci?
;;; string-suffix? string-suffix-ci?
;;; substring-prefix? substring-prefix-ci?
;;; substring-suffix? substring-suffix-ci?
;;; substring? substring-ci?
;;; string-fill! string-copy! string-copy substring
;;; string-reverse string-reverse! reverse-list->string
;;; string->list
;;; string-concat string-concat/shared string-append/shared
;;; xsubstring string-xcopy!
;;; string-null?
;;; join-strings
;;; 
;;; string? make-string string string-length string-ref string-set! 
;;; string-append list->string
;;;
;;; make-kmp-restart-vector
;;; parse-final-start+end
;;; parse-start+end
;;; check-substring-spec

;;; Imports
;;; This code has the following non-R5RS dependencies:
;;; - (RECEIVE (var ...) mv-exp body ...) multiple-value binding macro
;;; - Various imports from the char-set library
;;; - ERROR
;;; - LET-OPTIONALS and :OPTIONAL macros for handling optional arguments
;;; - The R5RS SUBSTRING function is accessed using the Scheme 48
;;;   STRUCTURE-REF magic accessor.

(define-module (scsh lib string-lib)
  :use-module (scsh alt-syntax)
  :use-module (ice-9 receive)
  :use-module (scsh let-opt)
  :use-module (scsh char-set)
)
(export string-map    string-map!
	string-fold       string-unfold
	string-fold-right string-unfold-right 
	string-tabulate
	string-for-each string-iter
	string-every string-any
	string-compare string-compare-ci
	substring-compare substring-compare-ci
	string= string< string> string<= string>= string<>
	string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
	substring=     substring<>		substring-ci=  substring-ci<>
	substring<     substring>		substring-ci<  substring-ci>
	substring<=    substring>=		substring-ci<= substring-ci>=
	string-upper-case? string-lower-case?
	capitalize-string  capitalize-words  string-downcase  string-upcase
	capitalize-string! capitalize-words! string-downcase! string-upcase!
	string-take string-take-right
	string-drop string-drop-right
	string-pad string-pad-right
	string-trim string-trim-right string-trim-both
	string-filter string-delete
	string-index string-index-right string-skip string-skip-right
	string-prefix-count string-prefix-count-ci
	string-suffix-count string-suffix-count-ci
	substring-prefix-count substring-prefix-count-ci
	substring-suffix-count substring-suffix-count-ci
	string-prefix? string-prefix-ci?
	string-suffix? string-suffix-ci?
	substring-prefix? substring-prefix-ci?
	substring-suffix? substring-suffix-ci?
	substring? substring-ci?
	string-fill! string-copy! string-copy ; substring
	string-reverse string-reverse! reverse-list->string
	string->list
	string-concat string-concat/shared string-append/shared
	xsubstring string-xcopy!
	; string-null?
	join-strings

	make-kmp-restart-vector
	parse-final-start+end
	parse-start+end
	check-substring-spec
)

;;; Support for START/END substring specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This macro parses optional start/end arguments from arg lists, defaulting
;;; them to 0/(string-length s), and checks them for correctness.

(define-syntax let-start+end
  (syntax-rules ()
    ((let-start+end (start end) proc s-exp args-exp body ...)
     (receive (start end) (parse-final-start+end proc s-exp args-exp)
       body ...))))


;;; Returns three values: start end rest

(define (parse-start+end proc s args)
  (let ((slen (string-length s)))
    (if (pair? args)

	(let ((start (car args))
	      (args (cdr args)))
	  (if (or (not (integer? start)) (< start 0))
	      (error "Illegal substring START spec" proc start s)
	      (receive (end args)
		  (if (pair? args)
		      (let ((end (car args))
			    (args (cdr args)))
			(if (or (not (integer? end)) (< slen end))
			    (error "Illegal substring END spec" proc end s)
			    (values end args)))
		      (values slen args))
		(if (<= start end) (values start end args)
		    (error "Illegal substring START/END spec"
			   proc start end s)))))

	(values 0 (string-length s) '()))))

(define (parse-final-start+end proc s args)
  (receive (start end rest) (parse-start+end proc s args)
    (if (pair? rest) (error "Extra arguments to procedure" proc rest)
	(values start end))))

(define (check-substring-spec proc s start end)
  (if (or (< start 0)
	  (< (string-length s) end)
	  (< end start))
      (error "Illegal substring START/END spec." proc s start end)))



;;; substring   S START [END] 
;;; string-copy S [START END]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefine SUBSTRING so that the END parameter is optional.
;;; SUBSTRINGX is the underlying R5RS SUBSTRING function. All
;;; the code in this file uses the simple SUBSTRINGX, so you can
;;; easily port this code.

;; guile's substring has optional 2nd arg.
;(define substringx (structure-ref scheme substring))	; Simple R5RS SUBSTRING
(define substringx substring)

;(define (substring s start . maybe-end)			; Our SUBSTRING
;  (substringx s start (:optional maybe-end (string-length s))))

(define (string-copy s . maybe-start+end)
  (let-start+end (start end) string-copy s maybe-start+end
    (substringx s start end)))



;;; Basic iterators and other higher-order abstractions
;;; (string-map proc s [start end])
;;; (string-map! proc s [start end])
;;; (string-fold kons knil s [start end])
;;; (string-fold-right kons knil s [start end])
;;; (string-unfold p f g seed)
;;; (string-for-each proc s [start end])
;;; (string-iter     proc s [start end])
;;; (string-every pred s [start end])
;;; (string-any pred s [start end])
;;; (string-tabulate proc len)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; No guarantees about order in MAP, FOR-EACH, EVERY, ANY procs.
;;;
;;; You want compiler support for high-level transforms on fold and unfold ops.
;;; You'd at least like a lot of inlining for clients of these procedures.
;;; Hold your breath.

(define (string-map proc s . maybe-start+end)
  (let-start+end (start end) string-map s maybe-start+end
    (let* ((len (- end start))
	   (ans (make-string len)))
      (do ((i (- end 1) (- i 1))
	   (j (- len 1) (- j 1)))
	  ((< j 0))
	(string-set! ans j (proc (string-ref s i))))
      ans)))

(define (string-map! proc s . maybe-start+end)
  (let-start+end (start end) string-map! s maybe-start+end
    (do ((i (- end 1) (- i 1)))
	((< i start))
      (string-set! s i (proc (string-ref s i))))))

(define (string-fold kons knil s . maybe-start+end)
  (let-start+end (start end) string-fold s maybe-start+end
    (let lp ((v knil) (i start))
      (if (< i end) (lp (kons (string-ref s i) v) (+ i 1))
	  v))))

(define (string-fold-right kons knil s . maybe-start+end)
  (let-start+end (start end) string-fold-right s maybe-start+end
    (let lp ((v knil) (i (- end 1)))
      (if (>= i start) (lp (kons (string-ref s i) v) (- i 1))
	  v))))

;;; (string-unfold p f g seed)
;;; This is the fundamental constructor for strings. 
;;; - G is used to generate a series of "seed" values from the initial seed:
;;;     SEED, (G SEED), (G^2 SEED), (G^3 SEED), ...
;;; - P tells us when to stop -- when it returns true when applied to one 
;;;   of these seed values.
;;; - F maps each seed value to the corresponding character 
;;;   in the result string.
;;;
;;; In other words, the following (simple, inefficient) definition holds:
;;; (string-unfold p f g seed) =
;;;   (if (p seed) "" 
;;;       (string-append (string (f seed)) 
;;;                      (string-unfold p f g (g seed))))
;;; 
;;; STRING-UNFOLD is a fairly powerful constructor -- you can use it to
;;; reverse a string, copy a string, convert a list to a string, read
;;; a port into a string, and so forth. Examples:
;;; (port->string port) =
;;;   (string-unfold (compose eof-object? peek-char)
;;;                  read-char identity port)
;;;
;;; (list->string lis) = (string-unfold null? car cdr lis)
;;; 
;;; (tabulate-string f size) = (string-unfold (lambda (i) (= i size)) f add1 0)

;;; A problem with the following simple formulation is that it pushes one
;;; stack frame for every char in the result string -- an issue if you are
;;; using it to read a 100kchar string. So we don't use it -- but I include
;;; it to give a clear, straightforward description of what the function
;;; does.

;(define (string-unfold p f g seed)
;  (let recur ((seed seed) (i 0))
;    (if (p seed) (make-string i)
;        (let* ((c (f seed))
;               (s (recur (g seed) (+ i 1))))
;          (string-set! s i c)
;          s))))

;;; This formulation chunks up the constructed string into 1024-char chunks,
;;; popping the stack frames. So it'll reduce stack growth by a factor of
;;; 1024. Marc Feeley alerted me to this issue and its general solution.

(define (string-unfold p f g seed)
  (string-concat/shared
	 (let recur ((seed seed))
	   (receive (s seed done?)
	            (let recur2 ((seed seed) (i 0))
		      (cond ((p seed) (values (make-string i) seed #t))
			    ((>= i 1024) (values (make-string i) seed #f))
			    (else (let ((c (f seed)))
				    (receive (s seed done?)
					     (recur2 (g seed) (+ i 1))
				      (string-set! s i c)
				      (values s seed done?))))))

	     (if done? (list s)
		 (cons s (recur seed)))))))


;;; This is the same as STRING-UNFOLD, but defined for multiple 
;;; seed parameters. If you pass N seeds, then
;;; - P maps N parameters to a boolean.
;;; - F maps N parameters to a character.
;;; - G maps N parameters to N return values.
;;; This definition does a lot of consing; it would need a fair amount
;;; of compiler support to be efficient.

; Not released
;(define (string-unfoldn p f g . seeds)
;  (apply string-append
;	 (let recur ((seeds seeds))
;	   (receive (s seeds done?)
;	            (let recur2 ((seeds seeds) (i 0))
;		      (cond ((apply p seeds) (values (make-string i) seeds #t))
;			    ((>= i 1024) (values (make-string i) seeds #f))
;			    (else (let ((c (apply f seeds)))
;				    (receive seeds (apply g seeds)
;				      (receive (s seeds done?)
;					       (recur2 seeds (+ i 1))
;					(string-set! s i c)
;					(values s seeds done?)))))))
;
;	     (if done? (list s)
;		 (cons s (recur seeds)))))))

(define (string-for-each proc s . maybe-start+end)
  (let-start+end (start end) string-for-each s maybe-start+end
    (do ((i (- end 1) (- i 1)))
	((< i start))
      (proc (string-ref s i)))))

(define (string-iter proc s . maybe-start+end)
  (let-start+end (start end) string-iter s maybe-start+end
    (do ((i start (+ i 1)))
	((>= i end))
      (proc (string-ref s i)))))

(define (string-every pred s . maybe-start+end)
  (let-start+end (start end) string-every s maybe-start+end
    (let lp ((i (- end 1)))
      (or (< i start)
	  (and (pred (string-ref s i))
	       (lp (- i 1)))))))

(define (string-any pred s . maybe-start+end)
  (let-start+end (start end) string-any s maybe-start+end
    (let lp ((i (- end 1)))
      (and (>= i start)
	   (or (pred (string-ref s i))
	       (lp (- i 1)))))))


(define (string-tabulate proc len)
  (let ((s (make-string len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0))
      (string-set! s i (proc i)))
    s))



;;; string-prefix-count[-ci] s1 s2
;;; string-suffix-count[-ci] s1 s2
;;; substring-prefix-count[-ci] s1 start1 end1  s2 start2 end2
;;; substring-suffix-count[-ci] s1 start1 end1  s2 start2 end2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find the length of the common prefix/suffix.
;;; It is not required that the two substrings passed be of equal length.
;;; This was microcode in MIT Scheme -- a very tightly bummed primitive.

(define (substring-prefix-count s1 start1 end1 s2 start2 end2)
  (check-substring-spec substring-prefix-count s1 start1 end1)
  (check-substring-spec substring-prefix-count s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))
    (let lp ((i start1) (j start2))
      (if (or (>= i end1)
	      (not (char=? (string-ref s1 i)
			   (string-ref s2 j))))
	  (- i start1)
	  (lp (+ i 1) (+ j 1))))))

(define (substring-suffix-count s1 start1 end1 s2 start2 end2)
  (check-substring-spec substring-suffix-count s1 start1 end1)
  (check-substring-spec substring-suffix-count s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (start1 (- end1 delta)))
    (let lp ((i (- end1 1)) (j (- end2 1)))
      (if (or (< i start1)
	      (not (char=? (string-ref s1 i)
			   (string-ref s2 j))))
	  (- (- end1 i) 1)
	  (lp (- i 1) (- j 1))))))

(define (substring-prefix-count-ci s1 start1 end1 s2 start2 end2)
  (check-substring-spec substring-prefix-count-ci s1 start1 end1)
  (check-substring-spec substring-prefix-count-ci s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))
    (let lp ((i start1) (j start2))
      (if (or (>= i end1)
	      (not (char-ci=? (string-ref s1 i)
			      (string-ref s2 j))))
	  (- i start1)
	  (lp (+ i 1) (+ j 1))))))

(define (substring-suffix-count-ci s1 start1 end1 s2 start2 end2)
  (check-substring-spec substring-suffix-count-ci s1 start1 end1)
  (check-substring-spec substring-suffix-count-ci s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (start1 (- end1 delta)))
    (let lp ((i (- end1 1)) (j (- end2 1)))
      (if (or (< i start1)
	      (not (char-ci=? (string-ref s1 i)
			      (string-ref s2 j))))
	  (- (- end1 i) 1)
	  (lp (- i 1) (- j 1))))))


(define (string-prefix-count s1 s2)
  (substring-prefix-count s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-suffix-count s1 s2)
  (substring-suffix-count s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-prefix-count-ci s1 s2)
  (substring-prefix-count-ci s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-suffix-count-ci s1 s2)
  (substring-suffix-count-ci s1 0 (string-length s1) s2 0 (string-length s2)))



;;; string-prefix?    s1 s2 
;;; string-suffix?    s1 s2 
;;; string-prefix-ci? s1 s2 
;;; string-suffix-ci? s1 s2 
;;; 
;;; substring-prefix?    s1 start1 end1 s2 start2 end2 
;;; substring-suffix?    s1 start1 end1 s2 start2 end2 
;;; substring-prefix-ci? s1 start1 end1 s2 start2 end2 
;;; substring-suffix-ci? s1 start1 end1 s2 start2 end2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are all simple derivatives of the previous counting funs.

(define (string-prefix? s1 s2) 
  (substring-prefix? s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-suffix? s1 s2) 
  (substring-suffix-ci? s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-prefix-ci? s1 s2)
  (substring-prefix-ci? s1 0 (string-length s1) s2 0 (string-length s2)))

(define (string-suffix-ci? s1 s2)
  (substring-suffix-ci? s1 0 (string-length s1) s2 0 (string-length s2)))

(define (substring-prefix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= (substring-prefix-count s1 start1 end1
				    s2 start2 end2)
	    len1))))

(define (substring-suffix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (substring-suffix-count s1 start1 end1
					 s2 start2 end2)))))

(define (substring-prefix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (substring-prefix-count-ci s1 start1 end1
					    s2 start2 end2)))))

(define (substring-suffix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (substring-suffix-count-ci s1 start1 end1
					    s2 start2 end2)))))


;;; string-compare    s1 s2 lt-proc eq-proc gt-proc
;;; string-compare-ci s1 s2 eq-proc lt-proc gt-proc
;;; substring-compare    s1 start1 end1 s2 start2 end2
;;;                      lt-proc eq-proc gt-proc
;;; substring-compare-ci s1 start1 end1 s2 start2 end2
;;;                      lt-proc eq-proc gt-proc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive string-comparison functions.
;;; Continuation order is different from MIT Scheme.
;;; Continuations are applied to s1's mismatch index;
;;; in the case of equality, this is END1.

(define (substring-compare s1 start1 end1 s2 start2 end2
			   proc< proc= proc>)
  (let ((size1 (- end1 start1))
	(size2 (- end2 start2)))
    (let ((match (substring-prefix-count s1 start1 end1 s2 start2 end2)))
      (if (= match size1)
	  ((if (= match size2) proc= proc<) end1)
	  ((if (= match size2)
	       proc>
	       (if (char<? (string-ref s1 (+ start1 match))
			   (string-ref s2 (+ start2 match)))
		   proc< proc>))
	   (+ match start1))))))

(define (substring-compare-ci s1 start1 end1 s2 start2 end2
			      proc< proc= proc>)
  (let ((size1 (- end1 start1))
	(size2 (- end2 start2)))
    (let ((match (substring-prefix-count-ci s1 start1 end1 s2 start2 end2)))
      (if (= match size1)
	  ((if (= match size2) proc= proc<) end1)
	  ((if (= match size2) proc>
	       (if (char-ci<? (string-ref s1 (+ start1 match))
			      (string-ref s2 (+ start2 match)))
		   proc< proc>))
	   (+ start1 match))))))

(define (string-compare s1 s2 proc< proc= proc>)
  (substring-compare s1 0 (string-length s1)
		     s2 0 (string-length s2)
		     proc< proc= proc>))

(define (string-compare-ci s1 s2 proc< proc= proc>)
  (substring-compare-ci s1 0 (string-length s1)
			s2 0 (string-length s2)
			proc< proc= proc>))


;;; string=          string<>		string-ci=          string-ci<>
;;; string<          string>		string-ci<          string-ci>
;;; string<=         string>=		string-ci<=         string-ci>=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple definitions in terms of the previous comparison funs.
;;; Inequality predicates return #f or mismatch index.
;;; I sure hope these defns get integrated.

(define (string=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare s1 s2 (lambda (i) #f) (lambda (i) i) (lambda (i) #f))))

(define (string<  s1 s2)
  (and (not (eq? s1 s2))		; Fast path
       (string-compare s1 s2 (lambda (i) i) (lambda (i) #f) (lambda (i) #f))))

(define (string>  s1 s2)
  (and (not (eq? s1 s2))		; Fast path
       (string-compare s1 s2 (lambda (i) #f) (lambda (i) #f) (lambda (i) i))))

(define (string<=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare s1 s2 (lambda (i) i) (lambda (i) i) (lambda (i) #f))))

(define (string>=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare s1 s2 (lambda (i) #f) (lambda (i) i) (lambda (i) i))))

(define (string<>  s1 s2) 
  (and (not (eq? s1 s2))		; Fast path
       (string-compare s1 s2 (lambda (i) i) (lambda (i) #f) (lambda (i) i))))


(define (string-ci=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare-ci s1 s2 (lambda (i) #f) (lambda (i) i) (lambda (i) #f))))

(define (string-ci<  s1 s2)
  (and (not (eq? s1 s2))		; Fast path
       (string-compare-ci s1 s2 (lambda (i) i) (lambda (i) #f) (lambda (i) #f))))

(define (string-ci>  s1 s2)
  (and (not (eq? s1 s2))		; Fast path
       (string-compare-ci s1 s2 (lambda (i) #f) (lambda (i) #f) (lambda (i) i))))

(define (string-ci<=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare-ci s1 s2 (lambda (i) i) (lambda (i) i) (lambda (i) #f))))

(define (string-ci>=  s1 s2)
  (if (eq? s1 s2) (string-length s1)	; Fast path
      (string-compare-ci s1 s2 (lambda (i) #f) (lambda (i) i) (lambda (i) i))))

(define (string-ci<>  s1 s2)
  (and (not (eq? s1 s2))		; Fast path
       (string-compare-ci s1 s2 (lambda (i) i) (lambda (i) #f) (lambda (i) i))))


(define (substring= s1 start1 end1 s2 start2 end2)
  (substring-compare s1 start1 end1
		     s2 start2 end2
		     (lambda (i) #f)
		     (lambda (i) i)
		     (lambda (i) #f)))

(define (substring<> s1 start1 end1 s2 start2 end2)
  (substring-compare s1 start1 end1
		     s2 start2 end2
		     (lambda (i) i)
		     (lambda (i) #f)
		     (lambda (i) i)))

(define (substring< s1 start1 end1 s2 start2 end2)
  (substring-compare s1 start1 end1
		     s2 start2 end2
		     (lambda (i) i)
		     (lambda (i) #f)
		     (lambda (i) #f)))

(define (substring> s1 start1 end1 s2 start2 end2)
  (substring< s2 start2 end2 s1 start1 end1))

(define (substring<= s1 start1 end1 s2 start2 end2)
  (substring-compare s1 start1 end1
		     s2 start2 end2
		     (lambda (i) i)
		     (lambda (i) i)
		     (lambda (i) #f)))

(define (substring>= s1 start1 end1 s2 start2 end2)
  (substring<= s2 start2 end2 s1 start1 end1))

(define (substring-ci= s1 start1 end1 s2 start2 end2)
  (substring-compare-ci s1 start1 end1
			s2 start2 end2
			(lambda (i) #f)
			(lambda (i) i)
			(lambda (i) #f)))

(define (substring-ci<> s1 start1 end1 s2 start2 end2)
  (substring-compare-ci s1 start1 end1
			s2 start2 end2
			(lambda (i) i)
			(lambda (i) #f)
			(lambda (i) i)))

(define (substring-ci< s1 start1 end1 s2 start2 end2)
  (substring-compare-ci s1 start1 end1
			s2 start2 end2
			(lambda (i) i)
			(lambda (i) #f)
			(lambda (i) #f)))

(define (substring-ci> s1 start1 end1 s2 start2 end2)
  (substring-ci< s2 start2 end2 s1 start1 end1))

(define (substring-ci<= s1 start1 end1 s2 start2 end2)
  (substring-compare-ci s1 start1 end1
			s2 start2 end2
			(lambda (i) i)
			(lambda (i) i)
			(lambda (i) #f)))

(define (substring-ci>= s1 start1 end1 s2 start2 end2)
  (substring-ci<= s2 start2 end2 s1 start1 end1))



;;; Case hacking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-upper-case?
;;; string-lower-case?
;;;
;;; string-upcase  s [start end]
;;; string-upcase! s [start end]
;;; string-downcase  s [start end]
;;; string-downcase! s [start end]
;;;
;;; capitalize-string  s [start end]
;;; capitalize-string! s [start end]
;;;   Uppercase first alphanum char, lowercase rest.
;;;
;;; capitalize-words  s [start end]
;;; capitalize-words! s [start end]
;;;   Capitalize every contiguous alphanum sequence: uppercase
;;;   first char, lowercase rest.

;;; These two use a different definition of an "upper-/lower-case string"
;;; than MIT Scheme uses:

(define (string-upper-case? s . maybe-start+end)
  (not (apply string-any char-lower-case? s maybe-start+end)))

(define (string-lower-case? s . maybe-start+end)
  (not (apply string-any char-upper-case? s maybe-start+end)))


(define (string-upcase  s . maybe-start+end)
  (apply string-map char-upcase s maybe-start+end))

(define (string-upcase! s . maybe-start+end)
  (apply string-map! char-upcase s maybe-start+end))

(define (string-downcase  s . maybe-start+end)
  (apply string-map char-downcase s maybe-start+end))

(define (string-downcase! s . maybe-start+end)
  (apply string-map! char-downcase s maybe-start+end))


;;; capitalize-string  s [start end]
;;; capitalize-string! s [start end]
;;;   Uppercase first alphanum char, lowercase rest.

(define (really-capitalize-string! s start end)
  (cond ((string-index s char-set:alphanumeric start end) =>
         (lambda (i)
	   (string-set! s i (char-upcase (string-ref s i)))
	   (string-downcase! s i)))))

(define (capitalize-string! s . maybe-start+end)
  (let-start+end (start end) capitalize-string! s maybe-start+end
    (really-capitalize-string! s start end)))

(define (capitalize-string s . maybe-start+end)
  (let-start+end (start end) capitalize-string s maybe-start+end
    (let ((ans (substringx s start end)))
      (really-capitalize-string! ans 0 (- end start))
      ans)))

;;; capitalize-words  s [start end]
;;; capitalize-words! s [start end]
;;;   Capitalize every contiguous alphanum sequence: uppercase
;;;   first char, lowercase rest.

(define (really-capitalize-words! s start end)
  (let lp ((i start))
    (cond ((string-index s char-set:alphanumeric i end) =>
           (lambda (i)
	     (string-set! s i (char-upcase (string-ref s i)))
	     (let ((i1 (+ i 1)))
	       (cond ((string-skip s char-set:alphanumeric i1 end) =>
		      (lambda (j)
			(string-downcase! s i1 j)
			(lp (+ j 1))))
		     (else (string-downcase! s i1 end)))))))))

(define (capitalize-words! s . maybe-start+end)
  (let-start+end (start end) capitalize-string! s maybe-start+end
    (really-capitalize-words! s start end)))

(define (capitalize-words s . maybe-start+end)
  (let-start+end (start end) capitalize-string! s maybe-start+end
    (let ((ans (substringx s start end)))
      (really-capitalize-words! ans 0 (- end start))
      ans)))



;;; Cutting & pasting strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-take string nchars
;;; string-drop string nchars
;;;
;;; string-pad string k [char start end] 
;;; string-pad-right string k [char start end] 
;;; 
;;; string-trim       string [char/char-set/pred start end] 
;;; string-trim-right string [char/char-set/pred start end] 
;;; string-trim-both  string [char/char-set/pred start end] 
;;;
;;; These trimmers invert the char-set meaning from MIT Scheme -- you
;;; say what you want to trim.

(define (string-take s n)
  (if (> n 0)
      (substringx s 0 n)
      (let ((len (string-length s)))
	(substringx s (+ len n) len))))

(define (string-drop s n)
  (let ((len (string-length s)))
    (if (> n 0)
	(substringx s n len)
	(substringx s 0 (+ len n)))))

(define (string-trim s . args)
  (let-optionals args ((criteria char-set:whitespace)
		       (start 0)
		       (end (string-length s)))
    (cond ((string-skip s criteria start end) =>
	   (lambda (i) (substringx s i end)))
	  (else ""))))

(define (string-trim-right s . args)
  (let-optionals args ((criteria char-set:whitespace)
		       (start 0)
		       (end (string-length s)))
    (cond ((string-skip-right s criteria end start) =>
	   (lambda (i) (substringx s 0 (+ 1 i))))
	  (else ""))))

(define (string-trim-both s . args)
  (let-optionals args ((criteria char-set:whitespace)
		       (start 0)
		       (end (string-length s)))
    (cond ((string-skip s criteria start end) =>
	   (lambda (i) (substringx s i (+ 1 (string-skip-right s criteria end)))))
	  (else ""))))


(define (string-pad-right s n . args)
  (let-optionals args ((char #\space) (start 0) (end (string-length s)))
    (check-substring-spec string-pad-right s start end)
    (let ((len (- end start)))
      (cond ((= n len)						; No pad.
	     (if (zero? start) s (substringx s start end)))

	    ((< n len) (substringx s start (+ start n)))	; Trim.

	    (else (let ((ans (make-string n char)))
		    (string-copy! ans 0 s start end)
		    ans))))))

(define (string-pad s n . args)
  (let-optionals args ((char #\space) (start 0) (end (string-length s)))
    (check-substring-spec string-pad s start end)
    (let ((len (- end start)))
      (cond ((= n len)						; No pad.
	     (if (zero? start) s (substringx s start end)))

	    ((< n len) (substringx s  (- end n) end))		; Trim.

	    (else (let ((ans (make-string n char)))
		    (string-copy! ans (- n len) s start end)
		    ans))))))



;;; Filtering strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-delete char/char-set/pred string [start end]
;;; string-filter char/char-set/pred string [start end]
;;;
;;; If the filter criteria is a char or char-set, we scan the string twice
;;;   with string-fold -- once to determine the length of the result string, 
;;;   and once to do the filtered copy.
;;; If the filter criteria is a predicate, we don't do this double-scan
;;;   strategy, because the predicate might have side-effects or be very
;;;   expensive to compute. So we preallocate a temp buffer pessimistically,
;;;   and only do one scan over S. This is likely to be faster and more
;;;   space-efficient than consing a list.

(define (string-delete criteria s . maybe-start+end)
  (let-start+end (start end) string-delete s maybe-start+end
    (if (procedure? criteria)
	(let* ((slen (- end start))
	       (temp (make-string slen))
	       (ans-len (string-fold (lambda (c i)
				       (if (criteria c) i
					   (begin (string-set! temp i c)
						  (+ i 1))))
				     0 s start end)))
	  (if (= ans-len slen) temp (substringx temp 0 ans-len)))

	(let* ((cset (cond ((char-set? criteria) criteria)
			   ((char? criteria) (char-set criteria))
			   (else (error "string-delete criteria not predicate, char or char-set" criteria))))
	       (len (string-fold (lambda (c i) (if (char-set-contains? cset c)
						   i
						   (+ i 1)))
				 0 s start end))
	       (ans (make-string len)))
	  (string-fold (lambda (c i) (if (char-set-contains? cset c)
					 i
					 (begin (string-set! ans i c)
						(+ i 1))))
		       0 s start end)
	  ans))))

(define (string-filter criteria s . maybe-start+end)
  (let-start+end (start end) string-filter s maybe-start+end
    (if (procedure? criteria)
	(let* ((slen (- end start))
	       (temp (make-string slen))
	       (ans-len (string-fold (lambda (c i)
				       (if (criteria c)
					   (begin (string-set! temp i c)
						  (+ i 1))
					   i))
				     0 s start end)))
	  (if (= ans-len slen) temp (substringx temp 0 ans-len)))

	(let* ((cset (cond ((char-set? criteria) criteria)
			   ((char? criteria) (char-set criteria))
			   (else (error "string-delete criteria not predicate, char or char-set" criteria))))

	       (len (string-fold (lambda (c i) (if (char-set-contains? cset c)
						   (+ i 1)
						   i))
				 0 s start end))
	       (ans (make-string len)))
	  (string-fold (lambda (c i) (if (char-set-contains? cset c)
					 (begin (string-set! ans i c)
						(+ i 1))
					 i))
		       0 s start end)
	  ans))))



;;; String search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-index       string char/char-set/pred [start end]
;;; string-index-right string char/char-set/pred [end start]
;;; string-skip        string char/char-set/pred [start end]
;;; string-skip-right  string char/char-set/pred [end start]
;;;     Note the odd start/end ordering of index-right and skip-right params.
;;;     There's a lot of replicated code here for efficiency.
;;;     For example, the char/char-set/pred discrimination has
;;;     been lifted above the inner loop of each proc.

(define (string-index str criteria . maybe-start+end)
  (let-start+end (start end) string-index str maybe-start+end
    (cond ((char? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (char=? criteria (string-ref str i)) i
		      (lp (+ i 1))))))
	  ((char-set? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (char-set-contains? criteria (string-ref str i)) i
		      (lp (+ i 1))))))
	  ((procedure? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (criteria (string-ref str i)) i
		      (lp (+ i 1))))))
	  (else (error "Second param is neither char-set, char, or predicate procedure."
		       string-index criteria)))))

(define (string-index-right str criteria . maybe-end+start)
  (let-optionals maybe-end+start ((start 0) (end (string-length str)))
    (check-substring-spec string-index-right str start end)
    (cond ((char? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (char=? criteria (string-ref str i)) i
		      (lp (- i 1))))))
	  ((char-set? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (char-set-contains? criteria (string-ref str i)) i
		      (lp (- i 1))))))
	  ((procedure? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (criteria (string-ref str i)) i
		      (lp (- i 1))))))
	  (else (error "Second param is neither char-set, char, or predicate procedure."
		       string-index-right criteria)))))

(define (string-skip str criteria . maybe-start+end)
  (let-start+end (start end) string-skip str maybe-start+end
    (cond ((char? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (char=? criteria (string-ref str i))
		      (lp (+ i 1))
		      i))))
	  ((char-set? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (char-set-contains? criteria (string-ref str i))
		      (lp (+ i 1))
		      i))))
	  ((char-set? criteria)
	   (let lp ((i start))
	     (and (< i end)
		  (if (criteria (string-ref str i)) (lp (+ i 1))
		      i))))
	  (else (error "Second param is neither char-set, char, or predicate procedure."
		       string-skip criteria)))))

(define (string-skip-right str criteria . maybe-end+start)
  (let-optionals maybe-end+start ((start 0) (end (string-length str)))
    (check-substring-spec string-index-right str start end)
    (cond ((char? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (char=? criteria (string-ref str i))
		      (lp (- i 1))
		      i))))
	  ((char-set? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (char-set-contains? criteria (string-ref str i))
		      (lp (- i 1))
		      i))))
	  ((procedure? criteria)
	   (let lp ((i (- end 1)))
	     (and (>= i 0)
		  (if (criteria (string-ref str i)) (lp (- i 1))
		      i))))
	  (else (error "CRITERIA param is neither char-set or char."
		       string-skip-right criteria)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-fill! string char [start end]
;;; 
;;; string-copy! to tstart from [fstart fend]
;;; 	Guaranteed to work, even if s1 eq s2.

(define (string-fill! s char . maybe-start+end)
  (let-start+end (start end) string-fill! s maybe-start+end
    (do ((i (- end 1) (- i 1)))
	((< i start))
      (string-set! s i char))))

(define (string-copy! to tstart from . maybe-fstart+fend)
  (let-start+end (fstart fend) string-copy! from maybe-fstart+fend
    (let ((tend (+ tstart (- fend fstart))))
      (check-substring-spec string-copy! to tstart tend)
      (if (> fstart tstart)
	  (do ((i fstart (+ i 1))
	       (j tstart (+ j 1)))
	      ((>= i fend))
	    (string-set! to j (string-ref from i)))

	  (do ((i (- fend 1) (- i 1))
	       (j (- tend 1) (- j 1)))
	      ((< i fstart))
	    (string-set! to j (string-ref from i)))))))



;;; Returns starting-position or #f if not true.
;;; This implementation is slow & simple. See below for KMP.
;;; Boyer-Moore would be nice.
;(define (substring? substring string . maybe-start+end)
;  (let-start+end (start end) string substring? maybe-start+end
;    (if (string-null? substring) start
;	(let* ((len (string-length substring))
;	       (i-bound (- end len))
;	       (char1 (string-ref substring start)))
;	  (let lp ((i 0))
;	    (cond ((string-index string char1 i i-bound) =>
;		   (lambda (i)
;		     (if (substring= substring 0 len string i (+ i len))
;			 i
;			 (lp (+ i 1)))))
;		  (else #f)))))))


;;; Searching for an occurence of a substring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This uses the KMP algorithm
;;;     "Fast Pattern Matching in Strings"
;;;     SIAM J. Computing 6(2):323-350 1977
;;;     D. E. Knuth, J. H. Morris and V. R. Pratt
;;; also described in
;;;     "Pattern Matching in Strings"
;;;     Alfred V. Aho
;;;     Formal Language Theory - Perspectives and Open Problems
;;;     Ronald V. Brook (editor)
;;; This algorithm is O(m + n) where m and n are the 
;;; lengths of the pattern and string respectively
;;; Original version of this code by bevan; I have substantially rehacked it.

(define (substring? pattern source . maybe-start+end)
  (let-start+end (start end) substring? source maybe-start+end
    (really-substring? char=? pattern source start end)))

(define (substring-ci? pattern source . maybe-start+end)
  (let-start+end (start end) substring-ci? source maybe-start+end
    (really-substring? char-ci=? pattern source start end)))

;;; Compute the Knuth-Morris-Pratt restart vector RV for string PATTERN.  If
;;; we have matched chars 0..i-1 of PATTERN against a search string S, and
;;; PATTERN[i] doesn't match S[k], then reset i := RV[i], and try again to
;;; match S[k].  If RV[i] = -1, then punt S[k] completely, and move on to
;;; S[k+1] and PATTERN[0].
;;;
;;; In other words, if you have matched the first i chars of PATTERN, but
;;; the i+1'th char doesn't match, RV[i] tells you what the next-longest
;;; prefix of PATTERN is that you have matched.
;;;
;;; C= is the character comparator -- usefully CHAR= or CHAR-CI=.
;;;
;;; I've split this out as a separate function in case other constant-string
;;; searchers might want to use it.

(define (make-kmp-restart-vector pattern c=)
  (let* ((plen (string-length pattern))
	 (rv (make-vector plen)))
    (if (> plen 0)
	(let ((plen-1 (- plen 1)))
	  (vector-set! rv 0 -1)
	  (let lp ((i 0) (j -1))
	    (if (< i plen-1)
		(if (or (= j -1)
			(c= (string-ref pattern i)
			    (string-ref pattern j)))
		    (let ((i (+ 1 i))
			  (j (+ 1 j)))
		      (vector-set! rv i j)
		      (lp i j))
		    (lp i (vector-ref rv j)))))))
    rv))

(define (really-substring? c= pattern source start end)
  (let ((plen (string-length pattern))
	(rv (make-kmp-restart-vector pattern c=)))

    ;; The search loop. SJ & PJ are redundant state.
    (let lp ((si start) (pi 0)
	     (sj (- end start))	; (- end si)  -- how many chars left.
	     (pj plen))		; (- plen pi) -- how many chars left.

      (if (= pi plen) (- si plen)			; Win.

	  (and (<= pj sj)				; Lose.
		     
	       (if (c= (string-ref source si)		; Search.
		       (string-ref pattern pi))
		   (lp (+ 1 si) (+ 1 pi) (- sj 1) (- pj 1))	; Advance.
			 
		   (let ((pi (vector-ref rv pi)))		; Retreat.
		     (if (= pi -1)
			 (lp (+ si 1)  0   (- sj 1)  plen)	; Punt.
			 (lp si        pi  sj        (- plen pi))))))))))



;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (string-reverse  s [start end])
;;; (string-reverse! s [start end])
;;; (string-null? s)

; guile's got this.
;(define (string-null? s) (zero? (string-length s)))

(define (string-reverse s . maybe-start+end)
  (let-start+end (start end) string-reverse s maybe-start+end
    (let ((ans (make-string (- end start))))
      (do ((i (- end 1) (- i 1))
	   (j start (+ j 1)))
	  ((< i j))
	(string-set! ans i (string-ref s j))
	(string-set! ans j (string-ref s i)))
      ans)))

(define (string-reverse! s . maybe-start+end)
  (let-start+end (start end) string-reverse! s maybe-start+end
    (do ((i (- end 1) (- i 1))
	 (j start (+ j 1)))
	((<= i j))
      (let ((ci (string-ref s i)))
	(string-set! s i (string-ref s j))
	(string-set! s j ci)))))


(define (reverse-list->string clist)
  (let* ((len (length clist))
	 (s (make-string len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
	((not (pair? clist)))
      (string-set! s i (car clist)))
    s))


;(define (string->list s . maybe-start+end)
;  (let-start+end (start end) string->list s maybe-start+end
;    (do ((i (- end 1) (- i 1))
;	 (ans '() (cons (string-ref s i) ans)))
;	((< i start) ans))))

(define (string->list s . maybe-start+end)
  (apply string-fold-right cons '() s maybe-start+end))

;;; string-concat        string-list -> string
;;; string-concat/shared string-list -> string
;;; string-append/shared s ... -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRING-APPEND/SHARED has license to return a string that shares storage
;;; with any of its arguments. In particular, if there is only one non-empty
;;; string amongst its parameters, it is permitted to return that string as
;;; its result. STRING-APPEND, by contrast, always allocates new storage.
;;;
;;; STRING-CONCAT & STRING-CONCAT/SHARED are passed a list of strings,
;;; which they concatenate into a result string. STRING-CONCAT always
;;; allocates a fresh string; STRING-CONCAT/SHARED may (or may not) return
;;; a result that shares storage with any of its arguments. In particular,
;;; if it is applied to a singleton list, it is permitted to return the
;;; car of that list as its value.
;;;
;;; This is portable code, but could be much more efficient w/compiler
;;; support. Especially the n-ary guys.

;;; We delete the empty strings from the parameter list before handing
;;; off to string-concat/shared. I wrote the recursion out by hand instead
;;; of using list-lib's FILTER or FILTER! to minimize non-R5RS dependencies.

(define (string-append/shared . strings) (string-concat/shared strings))

(define (string-concat/shared strings)
  (let ((strings (let recur ((strings strings))		; Delete empty strings.
		   (if (pair? strings)
		       (let ((s (car strings))
			     (tail (recur (cdr strings))))
			 (if (string-null? s) tail (cons s tail)))
		       '()))))

    (cond ((not (pair? strings)) "")			; () => "".
	  ((not (pair? (cdr strings))) (car strings))	; (s) => s.
	  (else (string-concat strings)))))		; Allocate & concat.

; Alas, Scheme 48's APPLY blows up if you have many, many arguments.
;(define (string-concat strings) (apply string-append strings))

;;; Here it is written out. I avoid using REDUCE to add up string lengths
;;; to avoid non-R5RS dependencies.
(define (string-concat strings)
  (let* ((total (do ((strings strings (cdr strings))
		     (i 0 (+ i (string-length (car strings)))))
		    ((not (pair? strings)) i)))
	 (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
	  (let ((s (car strings)))
	    (string-copy! ans i s)
	    (lp (+ i (string-length s)) (cdr strings)))))
    ans))
	  



;;; xsubstring s from [to start end] -> string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; S is a string; START and END are optional arguments that demarcate
;;; a substring of S, defaulting to 0 and the length of S (e.g., the whole
;;; string). Replicate this substring up and down index space, in both the
;;  positive and negative directions. For example, if S = "abcdefg", START=3, 
;;; and END=6, then we have the conceptual bidirectionally-infinite string
;;;     ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f ...
;;;     ... -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 ...
;;; XSUBSTRING returns the substring of this string beginning at index FROM,
;;; and ending at TO (which defaults to FROM+(END-START)).
;;; 
;;; You can use XSUBSTRING in many ways:
;;; - To rotate a string left:  (xsubstring "abcdef" 2)  => "cdefab"
;;; - To rotate a string right: (xsubstring "abcdef" -2) => "efabcd"
;;; - To replicate a string:    (xsubstring "abc" 0 7) => "abcabca"
;;;
;;; Note that 
;;;   - The FROM/TO indices give a half-open range -- the characters from
;;;     index FROM up to, but not including index TO.
;;;   - The FROM/TO indices are not in terms of the index space for string S.
;;;     They are in terms of the replicated index space of the substring
;;;     defined by S, START, and END.
;;;
;;; It is an error if START=END -- although this is allowed by special
;;; dispensation when FROM=TO.

(define (xsubstring s from . maybe-to+start+end)
  (receive (to start end)
           (if (pair? maybe-to+start+end)
	       (let-start+end (start end) xsubstring s (cdr maybe-to+start+end)
		 (values (car maybe-to+start+end) start end))
	       (let ((slen (string-length s)))
		 (values (+ from slen) 0 slen)))
    (let ((slen   (- end start))
	  (anslen (- to  from)))
      (cond ((< anslen 0)
	     (error "Illegal FROM/TO spec passed to xsubstring -- FROM > TO."
		    s from to start end))

	    ((zero? anslen) "")
	    ((zero? slen) (error "Empty (sub)string passed to xsubstring"
				  s from to start end))

	    ((= 1 slen)		; Fast path for 1-char replication.
	     (make-string anslen (string-ref s start)))

	    ;; Selected text falls entirely within one span.
	    ((= (floor (/ from slen)) (floor (/ to slen)))
	     (substringx s (+ start (modulo from slen))
			   (+ start (modulo to   slen))))

	    ;; Selected text requires multiple spans.
	    (else (let ((ans (make-string anslen)))
		    (multispan-repcopy! ans 0 s from to start end)
		    ans))))))


;;; string-xcopy! target tstart s sfrom [sto start end] -> unspecific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exactly the same as xsubstring, but the extracted text is written
;;; into the string TARGET starting at index TSTART.
;;; This operation is not defined if (EQ? TARGET S) -- you cannot copy
;;; a string on top of itself.

(define (string-xcopy! target tstart s sfrom . maybe-sto+start+end)
  (receive (sto start end)
           (if (pair? maybe-sto+start+end)
	       (let-start+end (start end) string-xcopy! s (cdr maybe-sto+start+end)
		 (values (car maybe-sto+start+end) start end))
	       (let ((slen (string-length s)))
		 (values (+ sfrom slen) 0 slen)))

    (let* ((tocopy (- sto sfrom))
	   (tend (+ tstart tocopy))
	   (slen (- end start)))
      (check-substring-spec string-xcopy! target tstart tend)
      (cond ((< tocopy 0)
	     (error "Illegal FROM/TO spec passed to string-xcopy! -- FROM > TO."
		    target tstart s sfrom sto start end))
	    ((zero? tocopy))
	    ((zero? slen) (error "Empty (sub)string passed to string-xcopy!"
				  target tstart s sfrom sto start end))

	    ((= 1 slen)			; Fast path for 1-char replication.
	     (string-fill! target (string-ref s start) tstart tend))

	    ;; Selected text falls entirely within one span.
	    ((= (floor (/ sfrom slen)) (floor (/ sto slen)))
	     (string-copy! target tstart s 
			   (+ start (modulo sfrom slen))
			   (+ start (modulo sto   slen))))

	    ;; Multi-span copy.
	    (else (multispan-repcopy! target tstart s sfrom sto start end))))))

;;; This is the core copying loop for XSUBSTRING and STRING-XCOPY!
;;; Internal -- not exported, no careful arg checking.
(define (multispan-repcopy! target tstart s sfrom sto start end)
  (let* ((slen (- end start))
	 (i0 (+ start (modulo sfrom slen)))
	 (total-chars (- sto sfrom)))

    ;; Copy the partial span @ the beginning
    (string-copy! target tstart s i0 end)
		    
    (let* ((ncopied (- end i0))			; We've copied this many.
	   (nleft (- total-chars ncopied))	; # chars left to copy.
	   (nspans (quotient nleft slen)))	; # whole spans to copy
			   
      ;; Copy the whole spans in the middle.
      (do ((i (+ tstart ncopied) (+ i slen))	; Current target index.
	   (nspans nspans (- nspans 1)))	; # spans to copy
	  ((zero? nspans)
	   ;; Copy the partial-span @ the end & we're done.
	   (string-copy! target i s start (+ start (- total-chars (- i tstart)))))

	(string-copy! target i s start end))))) ; Copy a whole span.



;;; (join-strings string-list [delimiter grammar]) => string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paste strings together using the delimiter string.
;;;
;;; (join-strings '("foo" "bar" "baz") ":") => "foo:bar:baz"
;;;
;;; DELIMITER defaults to a single space " "
;;; GRAMMAR is one of the symbols {infix, suffix} and defaults to 'infix.

;;; (join-strings strings [delim grammar])

(define (join-strings strings . args)
  (if (pair? strings)
      (let-optionals args ((delim " ") (grammar 'infix))
	(let ((strings (reverse strings)))
	  (let lp ((strings (cdr strings))
		   (ans (case grammar
			  ((infix)  (list (car strings)))
			  ((suffix) (list (car strings) delim))
			  (else (error "Illegal join-strings grammar" grammar)))))
	    (if (pair? strings)
		(lp (cdr strings)
		    (cons (car strings) (cons delim ans)))
	  
		; All done
		(string-concat ans)))))

      ""))	; Special-cased for infix grammar.



;;; MIT Scheme copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy and modify this software, to
;;; redistribute either the original software or a modified version, and
;;; to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;; 
;;; 1. Any copy made of this software must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this software.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that the operation of
;;; this software will be error-free, and MIT is under no obligation to
;;; provide any services, by way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case.
