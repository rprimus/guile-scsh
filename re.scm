;;; Regular expression matching for scsh
;;; Copyright (c) 1994 by Olin Shivers.

;;; Parts rewritten for Guile.

(foreign-source
  "/* Make sure foreign-function stubs interface to the C funs correctly: */"
  "#include \"re1.h\""
  "" ""
  )

;;; Match data for regexp matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record regexp-match
  string	; The string against which we matched.
  start		; 10 elt vec
  end)		; 10 elt vec

(define (match:start match . maybe-index)
  (let ((i (:optional maybe-index 0)))
    (or (vector-ref (regexp-match:start match) i)
	(error match:start "No sub-match found." match i))))

(define (match:end match . maybe-index)
  (let ((i (:optional maybe-index 0)))
    (or (vector-ref (regexp-match:end match) i)
	(error match:start "No sub-match found." match i))))

(define (match:substring match . maybe-index)
  (let* ((i (:optional maybe-index 0))
	 (start (vector-ref (regexp-match:start match) i)))
    (if start
	(substring (regexp-match:string match)
		   start
		   (vector-ref (regexp-match:end match) i))
	(error match:substring "No sub-match found." match i))))


;;; Compiling regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-record %regexp
;  string	; The string form of the regexp.
;  bytes		; The compiled representation, stuffed into a Scheme string.
;  ((disclose self) (list "Regexp" (%regexp:string self))))

;(define regexp? %regexp?)

(define regexp? compiled-regexp?)

;(define (make-regexp pattern)
;  (receive (err len) (%regexp-compiled-length pattern)
;    (if err (error err make-regexp pattern)
;	(let ((buf (make-string len)))
;	  (%regexp-compile pattern buf)
;	  (make-%regexp pattern buf)))))

(define (make-regexp pattern)
  (regcomp pattern REG_EXTENDED))

(define-foreign %regexp-compiled-length (re_byte_len (string pattern))
  static-string	; Error msg or #f
  integer)	; number of bytes needed to compile REGEXP.

(define-foreign %regexp-compile (re_compile (string pattern)
					    (string-desc bytes))
  static-string) ; Error msg or #f


;;; Executing compiled regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regexp-exec regexp str . maybe-start)
  (let* ((start (:optional maybe-start 0))
	 (start-vec (make-vector 10 #f))
	 (end-vec (make-vector 10 #f))
	 (match (regexec regexp (make-shared-substring str start))))
    (and match
	 (let ((match-count (vector-length match)))
	   (do ((i 0 (+ i 1)))
	       ((= i match-count)
		(make-regexp-match str start-vec end-vec))
	     (vector-set! start-vec i (+ start (car (vector-ref match i))))
	     (vector-set! end-vec i (+ start (cdr (vector-ref match i)))))))))

(define-foreign %regexp-exec (re_exec (string-desc compiled-regexp)
				      (string s)
				      (integer start)
				      (vector-desc start-vec)
				      (vector-desc end-vec))
  static-string	; Error msg or #f
  bool)		; Matched?


;;; Compile&match regexps in one go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I could do this with the separate compile and execute procedures, 
;;; but I go straight to C just for fun.
;;; For Guile, we do it with the separate compile and execute procedures.

(define (string-match pattern string . maybe-start)
  (let ((rx (make-regexp pattern))
	(start (:optional maybe-start 0)))
    (regexp-exec rx string start)))

(define-foreign %string-match (re_match (string pattern)
					(string s)
					(integer start)
					(vector-desc start-vec)
					(vector-desc end-vec))
  static-string ; Error string or #f if all is ok.
  bool)		; match?



;;; Substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign %regexp-subst (re_subst (string-desc compiled-regexp)
					(string match)
					(string str)
					(integer start)
					(vector-desc start-vec)
					(vector-desc end-vec)
					(string-desc outbuf))
  static-string	; Error msg or #f
  integer)

(define-foreign %regexp-subst-len (re_subst_len (string-desc compiled-regexp)
						(string match)
						(string str)
						(integer start)
						(vector-desc start-vec)
						(vector-desc end-vec))
  static-string	; Error msg or #f
  integer)

;;; What does this do?

;(define (regexp-subst re match replacement)
;  (let ((cr (%regexp:bytes re))
;	(str       (regexp-match:string match))
;	(start-vec (regexp-match:start  match))
;	(end-vec   (regexp-match:end    match)))
;    (receive (err out-len) (%regexp-subst-len cr str replacement 0
;					      start-vec end-vec)
;      (if err (error err regexp-subst str replacement) ; More data here
;	  (let ((out-buf (make-string out-len)))
;	    (receive (err out-len) (%regexp-subst cr str replacement 0
;						  start-vec end-vec out-buf)
;	      (if err (error err regexp-subst str replacement)
;		  (substring out-buf 0 out-len))))))))

;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I do this one in C, I'm not sure why:
;;; It is used by MATCH-FILES.
;;; For Guile it's done in Scheme.

(define-foreign %filter-C-strings!
  (filter_stringvec (string pattern) ((C "char const ** ~a") cvec))
  static-string	; error message -- #f if no error.
  integer)	; number of files that pass the filter.

(define (%filter-C-strings! pattern vec)
  (let ((rx (make-regexp pattern))
	(len (vector-length vec)))
    (let loop ((i 0) (j 0))
      (if (= i len)
	  (values #f j)
	  (loop (+ i 1)
		(if (regexec rx (vector-ref vec i) #f)
		    (begin
		      (vector-set! vec j (vector-ref vec i))
		      (+ j 1))
		    j))))))

;;; Convert a string into a regex pattern that matches that string exactly --
;;; in other words, quote the special chars with backslashes.

(define (regexp-quote string)
  (let lp ((i (- (string-length string) 1))
	   (result '()))
    (if (< i 0) (list->string result)
	(lp (- i 1)
	    (let* ((c (string-ref string i))
		   (result (cons c result)))
	      (if (memv c '(#\[ #\] #\. #\* #\? #\( #\) #\| #\\ #\$ #\^ #\+))
		  (cons #\\ result)
		  result))))))
