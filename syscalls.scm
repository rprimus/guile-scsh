;;; POSIX system-call Scheme binding.
;;; Copyright (c) 1993 by Olin Shivers.

;; Only the subset from scsh that's useful in Guile, rewritten in places. 
;; Incomplete.

;;; User info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record user-info
  name uid gid home-dir shell

  ;; Make user-info records print like #{user-info shivers}.
  ((disclose ui)
   (list "user-info" (user-info:name ui))))

;; rewritten.
(define (user-info uid/name)
  (let ((info (getpw uid/name)))
    (make-user-info (passwd:name info)
		    (passwd:uid info)
		    (passwd:gid info)
		    (passwd:dir info)
		    (passwd:shell info))))
(define name->user-info user-info)
(define uid->user-info user-info)
  
;;; Derived functions

(define (->uid uid/name)
  (user-info:uid (user-info uid/name)))

(define (->username uid/name)
  (user-info:name (user-info uid/name)))

(define (%homedir uid/name)
  (user-info:home-dir (user-info uid/name)))


;;; Group info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record group-info
  name gid members

  ;; Make group-info records print like #{group-info wheel}.
  ((disclose gi) (list "group-info" (group-info:name gi))))

;; rewritten.
(define (group-info gid/name)
  (let ((info (getgr gid/name)))
    (make-group-info (group:name info)
		     (group:gid info)
		     (group:mem info))))

;;; Derived functions

(define (->gid name)
  (group-info:gid (group-info name)))

(define (->groupname gid)
  (group-info:name (group-info gid)))

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

