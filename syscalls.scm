;;; POSIX system-call Scheme binding.
;;; Copyright (c) 1993 by Olin Shivers.

;; Rewritten for Guile in places, incomplete.

(set! exit primitive-exit)

;;; Miscellaneous process state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Working directory

(define-foreign %chdir/errno
  (chdir (string directory))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%chdir dir) %chdir/errno)

;; primitive in Guile.
;;(define (chdir . maybe-dir)
;;  (let ((dir (:optional maybe-dir (home-dir))))
;;    (%chdir (ensure-file-name-is-nondirectory dir))))


(define-foreign cwd/errno (scheme_cwd)
  (to-scheme integer "False_on_zero") ; errno or #f
  string) ; directory (or #f on error)

(define-errno-syscall (cwd) cwd/errno
  dir)

(define cwd getcwd)

(if (not (defined? 'guile-pipe))
    (define guile-pipe pipe))
(set! pipe (lambda ()
	     (let ((rv (guile-pipe)))
	       (values (car rv) (cdr rv)))))

;;; UMASK

(define-foreign set-umask (umask (mode_t mask)) no-declare ; integer on SunOS
  mode_t)

;; primitive in Guile.
;;(define (umask)
;;  (let ((m (set-umask 0)))
;;    (set-umask m)
;;    m))

(define (set-umask newmask) (umask newmask))

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


;;; Environment manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (var . val) / "var=val" rep conversion:

(define (split-env-string var=val)
  (let ((i (index var=val #\=)))
    (if i (values (substring var=val 0 i)
		  (substring var=val (+ i 1) (string-length var=val)))
	(error "No \"=\" in environment string" var=val))))

(define (env-list->alist env-list)
  (map (lambda (var=val)
	 (call-with-values (lambda () (split-env-string var=val))
			   cons))
       env-list))

(define (alist->env-list alist)
  (map (lambda (var.val)
	 (string-append (car var.val) "=" (cdr var.val)))
       alist))

;;; ENV->ALIST

(define-foreign %load-env (scm_envvec)
  (C char**)	; char **environ
  fixnum)	; & its length.

;(define (env->list)
;  (receive (C-env nelts) (%load-env)
;    (vector->list (C-string-vec->Scheme C-env nelts))))

(define (env->alist) (env-list->alist (environ)))

;;; ALIST->ENV

(define-foreign %install-env/errno
  (install_env (vector-desc env-vec))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%install-env env-vec) %install-env/errno)

(define (alist->env alist)
  (environ (alist->env-list alist)))

;;; GETENV, PUTENV, SETENV

(define-foreign getenv (getenv (string var))
  static-string)

(foreign-source
 "#define errno_on_nonzero_or_false(x) ((x) ? ENTER_FIXNUM(errno) : SCHFALSE)"
 "" "")

;(define-foreign putenv/errno
;  (put_env (string var=val))
;  desc) ; #f or errno


;;; putenv takes a constant: const char *, cig can't figure that out..
(define-foreign putenv/errno
  (putenv (string-copy var=val))  no-declare
  (to-scheme integer errno_on_nonzero_or_false)) ; #f or errno

(define-foreign delete-env (delete_env (string var))
  ignore)

;; primitive in Guile.
;; (define (putenv var=val)
;;  (if (putenv/errno var=val)
;;      (error "malloc failure in putenv" var=val)))
;;
;; in Guile's boot-9.scm.
;; (define (setenv var val)
;;  (if val
;;      (putenv (string-append var "=" val))
;;      (delete-env var)))
