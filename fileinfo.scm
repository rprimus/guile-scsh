;;; Copyright (c) 1993, 1994 by Olin Shivers.

;;; needs to be modified for Guile.

;;; chase? true (the default) means if the file is a symlink, chase the link
;;; and report on the file it references. chase? = #f means check the actual
;;; file itself, even if it's a symlink.
;;; writeable means (1) file exists & is writeable OR (2) file doesn't exist
;;;     but directory is writeable.

;;; Return values:
;;; #f			Accessible
;;; search-denied	Can't stat
;;; permission		File exists but is protected
;;; 			(also for errno/rofs)
;;; no-directory	Some directory doesn't exist
;;; nonexistent		File itself doesn't exist
;;;
;;; Otherwise, signals an error.

(define (file-not-accessible? perms fd/port/fname . maybe-chase?)
  (let ((uid (user-effective-uid)))
    (and (not (zero? uid)) ; Root can do what he likes.
	 (with-errno-handler ((err data)
			      ((errno/acces) 'search-denied)
			      ((errno/noent) 'nonexistent)
			      ((errno/notdir) 'not-directory))

	   (and (let* ((info (apply file-info fd/port/fname maybe-chase?))
		       (acc (file-info:mode info)))
		  (cond ((= (file-info:uid info) (user-effective-uid)) ; User
			 (zero? (bitwise-and acc (arithmetic-shift perms 6))))
		      
			((= (file-info:gid info) (user-effective-gid)) ; Group
			 (zero? (bitwise-and acc (arithmetic-shift perms 3))))
			((memv (file-info:gid info) (user-supplementary-gids))
			 (zero? (bitwise-and acc (arithmetic-shift perms 3))))
		      
			(else				    ; Other
			 (zero? (bitwise-and acc perms)))))
		'permission)))))

;;;;;;

(define (file-not-readable?   fd/port/fname)  (file-not-accessible? 4 fd/port/fname))
(define (file-not-writable?   fd/port/fname)  (file-not-accessible? 2 fd/port/fname))
(define (file-not-executable? fd/port/fname)  (file-not-accessible? 1 fd/port/fname))

(define (file-readable?   fd/port/fname)  (not (file-not-readable?   fd/port/fname)))
(define (file-writable?   fd/port/fname)  (not (file-not-writable?   fd/port/fname)))
(define (file-executable? fd/port/fname)  (not (file-not-executable? fd/port/fname)))

;;; Spelling corrected.
(define file-not-writeable?
  (deprecated-proc file-not-writable? "file-not-writeable?"
		   "Use file-not-writable? instead"))

(define file-writeable?
  (deprecated-proc file-writable? "file-writeable?"
		   "Use file-writable? instead"))

;;;;;;

;;; Returns
;;; #f		   exists
;;; #t		   doesn't exist
;;; 'search-denied can't stat
;;; ...or signals an error

(define (file-not-exists? fd/port/fname . maybe-chase?)
  (with-errno-handler
      ((err data)
       ((errno/acces) 'search-denied)
       ((errno/noent errno/notdir) #t))
    (apply file-info fd/port/fname maybe-chase?)
    #f))

(define (file-exists? fd/port/fname . maybe-chase?)
  (not (apply file-not-exists? fd/port/fname maybe-chase?)))

;;;;;;

;;; stat and derived file-{mode,size,owner,group,times,inode,...} ops.

(define-simple-syntax (define-stat-proc proc info-slot)
  (define (proc fname/fd/port . maybe-chase?)
    (info-slot (apply file-info fname/fd/port maybe-chase?))))

(define-stat-proc file-type               file-info:type)
(define-stat-proc file-group              file-info:gid)
(define-stat-proc file-inode              file-info:inode)
(define-stat-proc file-last-access        file-info:atime)
(define-stat-proc file-last-mod           file-info:mtime)
(define-stat-proc file-last-status-change file-info:ctime)
(define-stat-proc file-mode               file-info:mode)
(define-stat-proc file-nlinks             file-info:nlinks)
(define-stat-proc file-owner              file-info:uid)
(define-stat-proc file-size               file-info:size)

(define (file-directory? fname/fd/port . maybe-chase?)
  (eq? 'directory (apply file-type fname/fd/port maybe-chase?)))

(define (file-fifo? fname/fd/port . maybe-chase?)
  (eq? 'fifo (apply file-type fname/fd/port maybe-chase?)))

(define (file-regular? fname/fd/port . maybe-chase?)
  (eq? 'regular (apply file-type fname/fd/port maybe-chase?)))

(define (file-socket? fname/fd/port . maybe-chase?)
  (eq? 'socket (apply file-type fname/fd/port maybe-chase?)))

(define (file-special? fname/fd/port . maybe-chase?)
  (let ((type (apply file-type fname/fd/port maybe-chase?)))
    (or (eq? 'block-special type) (eq? 'char-special type))))

(define (file-symlink? fname/fd/port)  ; No MAYBE-CHASE?, of course.
  (eq? 'symlink (file-type fname/fd/port #f)))
