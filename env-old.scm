;;; installed-scm-file

;;;;  Copyright (C) 1996 Free Software Foundation, Inc.
;;;;  Includes code from SCSH 0.4.4, Copyright (c) 1993 by Olin Shivers.

;;; Additional Unix interface definitions SCSH-style.

;;; Environment strings.
;;; primitives: getenv, putenv, environ

;;; FIXME: should getenv throw exception if variable not found?

(define (setenv name value)
  (putenv (string-append name "=" value)))

(define (env->alist)
  (let ((split (lambda (str)
		 (let ((len (string-length str)))
		   (let next-char ((pos 0))
		     (if (= pos len)
			 ;; represent missing '=' by #f.
			 (cons str #f)
			 (if (char=? (string-ref str pos) #\=)
			     (cons (substring str 0 pos)
				   (substring str (+ pos 1) len))
			     (next-char (+ pos 1)))))))))
    (map split
	 (environ))))
    
(define (alist->env alist)
  (environ 
   (map (lambda (pair)
	  (string-append (if (cdr pair)
			     (string-append (car pair) "=" (cdr pair))
			     ;; convert #f (back?) to a missing '='.
			     (car pair))))
	alist)))

(define (alist-delete key alist)
  (let next-pair ((rest alist)
		  (result ()))
    (if (null? rest)
	(reverse result)
	(next-pair (cdr rest)
		   (if (equal? (caar rest) key)
		       result
		       (cons (car rest) result))))))

(define (alist-update key val alist)
  (cons (cons key val) (alist-delete key alist)))

;alist-compress
;with-env*
;with-total-env*
;with-env
;with-total-env
;add-before
;add-after
