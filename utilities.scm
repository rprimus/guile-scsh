;;; Random useful utilities.
;;; Copyright (c) 1993 by Olin Shivers.

(define (del elt lis)
  (letrec ((del (lambda (lis)
		  (if (pair? lis)
		      (let* ((head (car lis))
			     (tail (cdr lis))
			     (new-tail (del tail)))
			(if (equal? head elt) new-tail
			    (if (eq? tail new-tail) lis
				(cons head new-tail))))
		      '()))))
    (del lis)))

(define (delete pred lis)
  (filter (lambda (x) (not (pred x))) lis))

(define (index str c . maybe-start)
  (let ((start (max 0 (:optional maybe-start 0)))
	(len (string-length str)))
    (do ((i start (+ 1 i)))
	((or (>= i len)
	     (char=? c (string-ref str i)))
	 (and (< i len) i)))))

(define (rindex str c . maybe-start)
  (let* ((len (string-length str))
	 (start (min (:optional maybe-start len)
		     len)))
    (do ((i (- start 1) (- i 1)))
	((or (< i 0)
	     (char=? c (string-ref str i)))
	 (and (>= i 0) i)))))

;;; (f (f (f zero x1) x2) x3)
;;; [Richard's does (f x3 (f x2 (f x1 zero)))
(define (reduce f zero l)
  (letrec ((lp (lambda (val rest)
		 (if (pair? rest) (lp (f val (car rest)) (cdr rest))
		     val))))
    (lp zero l)))
				      
(define (filter pred list)
  (letrec ((filter (lambda (list)
		     (if (pair? list)
			 (let* ((head (car list))
				(tail (cdr list))
				(new-tail (filter tail)))
			   (if (pred head)
			       (if (eq? tail new-tail) list
				   (cons head new-tail))
			       new-tail))
			 '()))))
    (filter list)))

(define (first pred list)
  (letrec ((lp (lambda (list)
		 (and (pair? list)
		      (let ((head (car list)))
			(if (pred head) head
			    (lp (cdr list))))))))
    (lp list)))

(define any first)

;;; Returns the first true value produced by PRED, not the list element
;;; that satisfied PRED.

(define (first? pred list)
  (letrec ((lp (lambda (list)
		 (and (pair? list)
		      (or (pred (car list))
			  (lp (cdr list)))))))
    (lp list)))

(define any? first?)

(define (every? pred list)
  (letrec ((lp (lambda (list)
		 (or (not (pair? list))
		     (and (pred (car list))
			  (lp (cdr list)))))))
    (lp list)))

(define (mapv f v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len) ans)
      (vector-set! ans i (f (vector-ref v i))))))

(define (mapv! f v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i len) v)
      (vector-set! v i (f (vector-ref v i))))))

(define (vector-every? pred v)
  (let lp ((i (- (vector-length v) 1)))
    (or (< i 0)
	(and (pred (vector-ref v i))
	     (lp (- i 1))))))

(define (copy-vector v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) ans)
      (vector-set! ans i (vector-ref v i)))))

(define (initialize-vector len init)
  (let ((v (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) v)
      (vector-set! v i (init i)))))

(define (check-arg pred val caller)
  (if (pred val) val
      (check-arg pred (error "Bad argument" val pred caller) caller)))

(define (conjoin f g)
  (lambda args (and (apply f args) (apply g args))))

(define (disjoin f g)
  (lambda args (or (apply f args) (apply g args))))

(define (negate f) (lambda args (not (apply f args))))

(define (compose f g)
  (lambda args (call-with-values (lambda () (apply g args)) f)))


(define (reverse! lis)
  (let lp ((lis lis) (prev '()))
    (if (not (pair? lis)) prev
	(let ((tail (cdr lis)))
	  (set-cdr! lis prev)
	  (lp tail lis)))))

(define call/cc call-with-current-continuation)

(define (deposit-bit-field bits mask field)
  (bitwise-ior (bitwise-and field mask)
	       (bitwise-and bits  (bitwise-not mask))))


(define (nth lis i)
  (if (< i 0) (error "nth: illegal list index" i)
      (let lp ((l lis) (i i))
	(if (pair? l)
	    (if (zero? i) (car l)
		(lp (cdr l) (- i 1)))
	    (error "nth: index too large" lis i)))))


(define (deprecated-proc proc name . maybe-preferred-msg)
  (let ((warned? #f))
    (lambda args
      (cond ((not warned?)
	     (set! warned? #t)
	     (apply warn
		    "Deprecated procedure (may not be supported in a future release)"
		    name
		    maybe-preferred-msg)))
      (apply proc args))))


(define (real->exact-integer x)
  (let ((f (round x)))
    (if (inexact? f) (inexact->exact f) f)))


;;; Copy string SOURCE into TARGET[start,...]

(define (string-replace! target start source)
  (let ((len (string-length source)))
    (do ((i (+ start len -1) (- i 1))
	 (j (- len 1) (- j 1)))
      ((< j 0) target)
      (string-set! target i (string-ref source j)))))


;;; Copy SOURCE[source-start, source-end) into TARGET[start,)

(define (substring-replace! target start source source-start source-end)
  (do ((i (+ start (- source-end source-start) -1) (- i 1))
       (j (- source-end 1) (- j 1)))
      ((< j source-start) target)
    (string-set! target i (string-ref source j))))


;;; Compute (... (f (f (f zero c0) c1) c2) ...)

(define (string-reduce f zero s)
  (let ((len (string-length s)))
    (let lp ((v zero) (i 0))
      (if (= i len)
	  v
	  (lp (f v (string-ref s i)) (+ i 1))))))
