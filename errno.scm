;;; define errno/perm for EPERM etc.

(defmacro maybe-define-eno (value)
  (let ((scsh-name (string->symbol
		    (string-append "errno/" 
				   (string-downcase! 
				    (let ((str (symbol->string value)))
				      (substring str 1
						 (string-length str))))))))
    `(if (defined? ',value)
	 (define ,scsh-name ,value))))

(maybe-define-eno EPERM)
(maybe-define-eno EINTR)
(maybe-define-eno EAGAIN)
(maybe-define-eno EWOULDBLOCK)
(maybe-define-eno EACCES)
(maybe-define-eno ENOENT)
(maybe-define-eno ENOTDIR)
(maybe-define-eno ECHILD)
(maybe-define-eno EEXIST)
(maybe-define-eno EBADF)

(undefine maybe-define-eno)
