;;; Basic read and write
;;; Copyright (c) 1993 by Olin Shivers.

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
	(receive (err nread) (reader s start end source)
	  (cond ((not err) (and (not (zero? nread)) nread))
		((= err errno/intr) (loop))
		((or (= err errno/wouldblock)	; No forward-progess here.
		     (= err errno/again))
		 0)
		(else (errno-error err reader s start start end source)))))))

(define (read-string!/partial s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
    (cond ((integer? fd/port)
	   (generic-read-string!/partial s start end
					 read-fdes-substring!/errno fd/port))
	  ((fdport? fd/port)
	   (generic-read-string!/partial s start end
					 read-fdport*-substring!/errno 
					 (extensible-port-local-data fd/port)))

	  (else ; Hack it for base S48 ports
	   ;; This case is a little gross in order to get 
	   ;; the forward-progress guarantee and handle non-blocking i/o.
	   ;; Unix sux. So do low-level Scheme looping constructs.
	   (if (>= start end) 0
	       (let lp ((i start))
		 (let ((c (with-errno-handler
			      ((err data) ((errno/wouldblock errno/again) #f))
			    (read-char fd/port))))
		   (cond ((not c) (- i start)) ; non-blocking i/o bailout
			 ((eof-object? c)
			  (let ((nread (- i start)))
			    (and (not (zero? nread)) nread)))
			 (else
			  (string-set! s i c)
			  (let ((i (+ i 1)))
			    (if (or (= i end) (not (char-ready? fd/port)))
				(- i start)
				(lp i))))))))))))

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
	(receive (err nread) (reader s i end source)
	  (cond (err (if (= err errno/intr) (loop i)
			 ;; Give info on partially-read data in error packet.
			 (errno-error err reader
				      s start i end source)))

		((zero? nread) ; EOF
		 (let ((result (- i start)))
		   (and (not (zero? result)) result)))

		(else (loop (+ i nread))))))))

(define (read-string! s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
    (cond ((integer? fd/port)
	   (generic-read-string! s start end
				 read-fdes-substring!/errno fd/port))

	  ((fdport? fd/port)
	   (generic-read-string! s start end
				 read-fdport*-substring!/errno
				 (extensible-port-local-data fd/port)))

	  ;; Hack it
	  (else (let lp ((i start))
		  (if (= i end) (- end start)
		      (let ((c (read-char fd/port)))
			(if (eof-object? c)
			    (let ((nread (- i start)))
			      (and (not (zero? nread)) nread))
			    (begin (string-set! s i c)
				   (lp (+ i 1)))))))))))

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
	(receive (err nwritten) (writer s start end target)
	  (cond ((not err) nwritten)
		((= err errno/intr) (loop))
		((or (= err errno/again) (= err errno/wouldblock)) 0)
		(else (errno-error err writer
				   s start start end target)))))))

(define (write-string/partial s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start 0)
		       (end (string-length s)))
    (cond ((integer? fd/port)
	   (generic-write-string/partial s start end
					 write-fdes-substring/errno fd/port))
	  ((fdport? fd/port)
	   (generic-write-string/partial s start end
					 write-fdport*-substring/errno
					 (extensible-port-local-data fd/port)))
	  (else (display (substring s start end) fd/port))))) ; hack


;;; Persistent writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-write-string s start end writer target)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" writer s start end target))

  (let loop ((i start))
    (if (< i end)
	(receive (err nwritten) (writer s i end target)
	  (cond ((not err) (loop (+ i nwritten)))
		((= err errno/intr) (loop i))
		(else (errno-error err writer
				   s start i end target)))))))

(define (write-string s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start   0)
		       (end     (string-length s)))
    (cond ((integer? fd/port)
	   (generic-write-string s start end
				 write-fdes-substring/errno fd/port))
	  ((fdport? fd/port)
	   (generic-write-string s start end
				 write-fdport*-substring/errno
				 (extensible-port-local-data fd/port)))

	  (else (display (substring s start end) fd/port))))) ; hack

(define (y-or-n? question . maybe-eof-value)
  (let loop ((count *y-or-n-eof-count*))
    (display question)
    (display " (y/n)? ")
    (let ((line (read-line)))
      (cond ((eof-object? line)
	     (newline)
	     (if (= count 0)
		 (:optional maybe-eof-value (error "EOF in y-or-n?"))
		 (begin (display "I'll only ask another ")
			(write count)
			(display " times.")
			(newline)
			(loop (- count 1)))))
	    ((< (string-length line) 1) (loop count))
	    ((char=? (string-ref line 0) #\y) #t)
	    ((char=? (string-ref line 0) #\n) #f)
	    (else (loop count))))))

(define *y-or-n-eof-count* 100)
