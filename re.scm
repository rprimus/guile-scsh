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
  (vector-ref (regexp-match:start match)
	      (:optional maybe-index 0)))

(define (match:end match . maybe-index)
  (vector-ref (regexp-match:end match)
	      (:optional maybe-index 0)))

(define (match:substring match . maybe-index)
  (let* ((i (:optional maybe-index 0))
	 (start (vector-ref (regexp-match:start match) i)))
    (and start (substring (regexp-match:string match)
			  start
			  (vector-ref (regexp-match:end match) i)))))

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

(define (regexp-substitute port match . items)
  (let* ((str (regexp-match:string match))
	 (sv (regexp-match:start match))
	 (ev (regexp-match:end match))
	 (range (lambda (item)			; Return start & end of
		  (cond ((integer? item)	; ITEM's range in STR.
			 (values (vector-ref sv item)
				 (vector-ref ev item)))
			((eq? 'pre item) (values 0 (vector-ref sv 0)))
			((eq? 'post item) (values (vector-ref ev 0)
						  (string-length str)))
			(else (error "Illegal substitution item."
				     item
				     regexp-substitute))))))
    (if port

	;; Output port case.
	(for-each (lambda (item)
		    (if (string? item) (write-string item port)
			(receive (si ei) (range item)
			  (write-string str port si ei))))
		  items)

	;; Here's the string case. Make two passes -- one to
	;; compute the length of the target string, one to fill it in.
	(let* ((len (reduce (lambda (i item)
			      (+ i (if (string? item) (string-length item)
				       (receive (si ei) (range item) (- ei si)))))
			    0 items))
	       (ans (make-string len)))

	  (reduce (lambda (index item)
		    (cond ((string? item)
			   (string-replace! ans index item)
			   (+ index (string-length item)))
			  (else (receive (si ei) (range item)
				  (substring-replace! ans index str si ei)
				  (+ index (- ei si))))))
		  0 items)
	  ans))))



(define (regexp-substitute/global port re str . items)
  (let ((range (lambda (start sv ev item)	; Return start & end of
		 (cond ((integer? item)		; ITEM's range in STR.
			(values (vector-ref sv item)
				(vector-ref ev item)))
		       ((eq? 'pre item) (values start (vector-ref sv 0)))
		       (else (error "Illegal substitution item."
				    item
				    regexp-substitute/global)))))
	(num-posts (reduce (lambda (count item)
			     (+ count (if (eq? item 'post) 1 0)))
			   0 items)))
    (if (and port (< num-posts 2))

	;; Output port case, with zero or one POST items.
	(let recur ((start 0))
	  (let ((match (string-match re str start)))
	    (if match
		(let* ((sv (regexp-match:start match))
		       (ev (regexp-match:end match)))
		  (for-each (lambda (item)
			      (cond ((string? item) (write-string item port))
				    ((procedure? item) (write-string (item match) port))
				    ((eq? 'post item) (recur (vector-ref ev 0)))
				    (else (receive (si ei)
					           (range start sv ev item)
					    (write-string str port si ei)))))
			    items))

		(write-string str port start)))) ; No match.

	(let* ((pieces (let recur ((start 0))
			 (let ((match (string-match re str start))
			       (cached-post #f))
			   (if match
			       (let* ((sv (regexp-match:start match))
				      (ev (regexp-match:end match)))
				 (reduce (lambda (pieces item)
					   (cond ((string? item)
						  (cons item pieces))

						 ((procedure? item)
						  (cons (item match) pieces))

						 ((eq? 'post item)
						  (if (not cached-post)
						      (set! cached-post
							    (recur (vector-ref ev 0))))
						  (append cached-post pieces))

						 (else (receive (si ei)
							   (range start sv ev item)
							 (cons (substring str si ei)
							       pieces)))))
					 '() items))

			       ;; No match. Return str[start,end].
			       (list (if (zero? start) str 
					 (substring str start (string-length str))))))))
	       (pieces (reverse pieces)))
	  (if port (for-each (lambda (p) (write-string p port)) pieces)
	      (apply string-append pieces))))))



;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;; Count the number of possible sub-matches in a regexp 
;;; (i.e., the number of left parens).

(define (regexp-num-submatches s)
  (let* ((len (string-length s))
	 (len-1 (- len 1)))
    (let lp ((i 0) (nsm 0))
      (if (= i len) nsm
	  (case (string-ref s i)
	    ((#\\) (if (< i len-1) (lp (+ i 2) nsm) nsm))
	    ((#\() (lp (+ i 1) (+ nsm 1)))
	    (else (lp (+ i 1) nsm)))))))
