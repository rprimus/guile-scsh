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

