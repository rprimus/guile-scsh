;;; Regular expression matching for scsh
;;; Copyright (c) 1994 by Olin Shivers.

;;; most of this file has been omitted since guile-core
;;; has compatible routines.

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
