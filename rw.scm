;;; Basic read and write
;;; Copyright (c) 1993 by Olin Shivers.
;;; modified to use Guile primitives.

;;; Note: read ops should check to see if their string args are mutable.

(define (bogus-substring-spec? s start end)
  (or (< start 0)
      (< (string-length s) end)
      (< end start)))


;;; Best-effort/forward-progress reading 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-read-string!/partial s start end reader source)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" reader source s start end))

  (if (= start end) 0 ; Vacuous request.
      (let loop ()
	(catch 'system-error
	       (lambda ()
		 (let ((nread (reader s source start end)))
		   (and (not (zero? nread)) nread)))
	       (lambda args 
		 (let ((err (car (list-ref args 4))))
		   (cond ;; ((= err errno/intr) (loop)) ; handled by primitive.
			((or (= err errno/wouldblock); No forward-progess here.
			     (= err errno/again))
			 0)
			(else (apply scm-error args)))))))))

(define (read-string!/partial s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
		 (generic-read-string!/partial s start end
					       uniform-array-read!
					       fd/port)))

(define (read-string/partial len . maybe-fd/port) 
  (let* ((s (make-string len))
	 (fd/port (:optional maybe-fd/port (current-input-port)))
	 (nread (read-string!/partial s fd/port 0 len)))
    (cond ((not nread) #f) ; EOF
	  ((= nread len) s)
	  (else (substring s 0 nread)))))


;;; Persistent reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-read-string! s start end reader source)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" reader source s start end))

  (let loop ((i start))
    (if (>= i end) (- i start)
	(catch 'system-error
	       (lambda ()
		 (let ((nread (reader s source i end)))
		   (if (zero? nread) ; EOF
		       (let ((result (- i start)))
			 (and (not (zero? result)) result))
		       (loop (+ i nread)))))
	       (lambda args
		 ;; Give info on partially-read data in error packet.
		 (set-cdr! (list-ref args 4) s)
		 (apply scm-error args))))))

(define (read-string! s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
		 (generic-read-string! s start end
				       uniform-array-read!
				       fd/port)))

(define (read-string len . maybe-fd/port) 
  (let* ((s (make-string len))
	 (fd/port (:optional maybe-fd/port (current-input-port)))
	 (nread (read-string! s fd/port 0 len)))
    (cond ((not nread) #f) ; EOF
	  ((= nread len) s)
	  (else (substring s 0 nread)))))


;;; Best-effort/forward-progress writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-blocking output to a buffered port is not defined.

(define (generic-write-string/partial s start end writer target)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" writer s start end target))

  (if (= start end) 0			; Vacuous request.
      (let loop ()
	(catch 'system-error
	       (lambda ()
		 (let ((nwritten (writer s target start end start)))
		   nwritten))
	       (lambda args 
		 (let ((err (car (list-ref args 4))))
		   (cond ;; ((= err errno/intr) (loop)) ; handled by primitive.
			((or (= err errno/wouldblock); No forward-progess here.
			     (= err errno/again))
			 0)
			(else (apply scm-error args)))))))))

(define (write-string/partial s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start 0)
		       (end (string-length s)))
		 (generic-write-string/partial s start end
					 uniform-array-write fd/port)))


;;; Persistent writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-write-string s start end writer target)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" writer s start end target))

  (let loop ((i start))
    (if (< i end)
	(catch 'system-error
	       (lambda ()
		 (let ((nwritten (writer s target i end i)))
		   (loop (+ i nwritten))))
	       (lambda args
		 (apply scm-error args))))))

(define (write-string s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start   0)
		       (end     (string-length s)))
		 (generic-write-string s start end
				 uniform-array-write fd/port)))

;(define (y-or-n? question . maybe-eof-value)
;  (let loop ((count *y-or-n-eof-count*))
;    (display question)
;    (display " (y/n)? ")
;    (let ((line (read-line)))
;      (cond ((eof-object? line)
;	     (newline)
;	     (if (= count 0)
;		 (:optional maybe-eof-value (error "EOF in y-or-n?"))
;		 (begin (display "I'll only ask another ")
;			(write count)
;			(display " times.")
;			(newline)
;			(loop (- count 1)))))
;	    ((< (string-length line) 1) (loop count))
;	    ((char=? (string-ref line 0) #\y) #t)
;	    ((char=? (string-ref line 0) #\n) #f)
;	    (else (loop count))))))

;(define *y-or-n-eof-count* 100)