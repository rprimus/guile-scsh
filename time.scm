;;; Time interface for scsh.
;;; Copyright (c) 1994 by Olin Shivers.

;;; Modified to use Guile primitives.

;;; Should I have a (FILL-IN-DATE! date) procedure that fills in
;;; the redundant info in a date record?
;;; - month-day & month defined -> week-day & year-day filled in.
;;; - month-day and year-day filled in from week-day and year-day
;;;   (not provided by mktime(), but can be synthesized)
;;; - If tz-secs and tz-name not defined, filled in from current time zone.
;;; - If tz-name not defined, fabbed from tz-secs.
;;; - If tz-secs not defined, filled in from tz-name.

(foreign-source "#include \"time1.h\""	; Import the time1.h interface.
		"")

;;; A TIME is an instant in the history of the universe; it is location
;;; independent, barring relativistic effects. It is measured as the
;;; number of seconds elapsed since "epoch" -- January 1, 1970 UTC.

;;; A DATE is a *local* name for an instant in time -- which instant
;;; it names depends on your time zone (February 23, 1994 4:37 pm happens 
;;; at different moments in Boston and Hong Kong).

;;; DATE definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We hack this so the date maker can take take the last three slots
;;; as optional arguments.

(define-record %date	; A Posix tm struct
  seconds	; Seconds after the minute (0-59)
  minute	; Minutes after the hour (0-59)
  hour   	; Hours since midnight (0-23)
  month-day	; Day of the month (1-31)
  month   	; Months since January (0-11)
  year    	; Years since 1900
  tz-name	; Time zone as a string.
  tz-secs	; Time zone as an integer: seconds west of UTC.
  summer?	; Summer time (Daylight savings) in effect?
  week-day	; Days since Sunday (0-6)	; Redundant
  year-day)	; Days since Jan. 1 (0-365)	; Redundant

(define date? %date?)

(define date:seconds	%date:seconds)
(define date:minute	%date:minute)
(define date:hour	%date:hour)
(define date:month-day	%date:month-day)
(define date:month	%date:month)
(define date:year	%date:year)
(define date:tz-name	%date:tz-name)
(define date:tz-secs	%date:tz-secs)
(define date:summer?	%date:summer?)
(define date:week-day	%date:week-day)
(define date:year-day	%date:year-day)

(define set-date:seconds	set-%date:seconds)
(define set-date:minute		set-%date:minute)
(define set-date:hour		set-%date:hour)
(define set-date:month-day	set-%date:month-day)
(define set-date:month		set-%date:month)
(define set-date:year		set-%date:year)
(define set-date:tz-name	set-%date:tz-name)
(define set-date:tz-secs	set-%date:tz-secs)
(define set-date:summer?	set-%date:summer?)
(define set-date:week-day	set-%date:week-day)
(define set-date:year-day	set-%date:year-day)

(define (make-date s mi h md mo y . args)
  (let-optionals args ((tzn #f) (tzs #f) (s?  #f) (wd  0)  (yd  0))
    (make-%date s mi h md mo y tzn tzs s? wd yd)))


;;; Not exported to interface.
(define (time-zone? x)
  (or (integer? x)	; Seconds offset from UTC.
      (string? x)	; Time zone name, e.g. "EDT"
      (not x)))		; Local time


;;; Time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ticks/sec 1000000)

(define-foreign %time+ticks/errno (time_plus_ticks)	; C fun is OS-dependent
  desc	  ; errno or #f
  fixnum  ; hi secs
  fixnum  ; lo secs
  fixnum  ; hi ticks
  fixnum) ; lo ticks

(define (time+ticks)
  (let ((rv (gettimeofday)))
    (values (car rv) (cdr rv))))

(define (time+ticks->time secs ticks)
  (+ secs (/ ticks ticks/sec)))

(define-foreign %time/errno (scheme_time)
  desc	  ; errno or #f
  fixnum  ; hi secs
  fixnum) ; lo secs


(define-foreign %date->time/errno (date2time (fixnum sec)
					     (fixnum min)
					     (fixnum hour)
					     (fixnum month-day)
					     (fixnum month)
					     (fixnum year)
					     (desc   tz-name)	; #f or string
					     (desc   tz-secs)	; #f or int
					     (bool   summer?))
  desc	  ; errno or #f
  fixnum  ; hi secs
  fixnum) ; lo secs

;(define (time . args) ; optional arg [date]
;  (let lp ()
;    (receive (err hi-secs lo-secs)
;	(if (null? args)
;	    (%time/errno)		; Fast path for (time).
;	    (let ((date (check-arg date? (car args) time)))
;	      (%date->time/errno (date:seconds   date)
;				 (date:minute    date)
;				 (date:hour      date)
;				 (date:month-day date)
;				 (date:month     date)
;				 (date:year      date)
;				 (date:tz-name   date) ; #f or string
;				 (date:tz-secs   date) ; #f or int
;				 (date:summer?   date))))

;      (cond ((not err) (compose-8/24 hi-secs lo-secs))	; Win.
;	    ((= errno/intr err) (lp))			; Retry.
;	    (else (apply errno-error err time args)))))); Lose.


;;; Date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-foreign %time->date (time2date (fixnum time-hi)
				       (fixnum time-lo)
				       (desc zone))
  desc		; errno or #f
  fixnum	; seconds
  fixnum	; minute
  fixnum	; hour
  fixnum	; month-day
  fixnum	; month
  fixnum	; year
  string	; tz-name (#f if we need to make it from tz-secs)
  fixnum	; tz-secs
  bool		; summer?
  fixnum	; week-day
  fixnum)	; year-day


;(define (date . args)	; Optional args [time zone]
;  (let* ((time (if (pair? args)
;		   (car args)
;		   (time)))
;	 (zone (check-arg time-zone?
;			  (and (pair? args) (:optional (cdr args) #f))
;			  date))
;	 (bt (if zone
;		 (localtime time zone)
;		 (localtime time))))
;    (make-%date (tm:sec bt) (tm:min bt) (tm:hour bt) (tm:mday bt)
;		(tm:mon bt) (tm:year bt)
;		(format-time-zone (tm:zone bt) (tm:gmtoff bt))
;		(tm:gmtoff bt) (= (tm:isdst bt) 1) (tm:wday bt) 
;		(tm:yday bt))))

;;; Formatting date strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (date->string date)	; Sun Sep 16 01:03:52 1973
  (format-date "~a ~b ~d ~H:~M:~S ~Y" date))

;(define (format-date fmt date)
;  (check-arg date? date format-date)
;  (receive (err result)
;	   (%format-date/errno fmt
;			       (date:seconds   date)
;			       (date:minute    date)
;			       (date:hour      date)
;			       (date:month-day date)
;			       (date:month     date)
;			       (date:year      date)
;			       (if (string? (date:tz-name date))
;				   (date:tz-name date)
;				   (deintegerize-time-zone (date:tz-secs date)))
;			       (date:summer?   date)
;			       (date:week-day  date)
;			       (date:year-day  date))
;    (cond ((not err) result)
;	  ((= errno/intr err) (format-date fmt date))
;	  (else (errno-error err format-date fmt date)))))

(define-foreign %format-date/errno (format_date (string fmt)
						(fixnum seconds)
						(fixnum minute)
						(fixnum hour)
						(fixnum month-day)
						(fixnum month)
						(fixnum year)
						(desc   tz-name)
						(bool   summer?)
						(fixnum week-day)
						(fixnum year-day))
  desc		; false or errno
  string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Obsoleted, since DATE records now include time zone info.
;;; If you want the UTC offset, just do (date:tz-secs (date [time tz])).
;;;
;(define (utc-offset . args) ; Optional args [time tz]
;  (let ((tim (if (pair? args)
;		 (real->exact-integer (check-arg real? (car args) utc-offset))
;		 (time)))
;	(tz (and (pair? args)
;		 (check-arg time-zone? (:optional (cdr args) #f) utc-offset))))
;    (if (integer? tz) tz
;	(- (time (date tim tz) 0) tim))))


;(define (time-zone . args)	; Optional args [summer? tz]
;  (let ((tz (and (pair? args)
;		 (check-arg time-zone? (:optional (cdr args) #f) time-zone))))
;    (if (integer? tz)
;	(deintegerize-time-zone tz)
;	(let* ((summer? (if (pair? args) (car args) (time)))
;	       (summer? (if (real? summer?) (real->exact-integer summer?) summer?)))
;	  (receive (err zone) (%time-zone/errno summer? tz)
;		   (if err (errno-error err time-zone summer? tz)
;	    zone))))))
		 
;;; 8/24 bit signed integer splitting and recombination.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (hi8  n) (bitwise-and (arithmetic-shift n -24) #xff))
;(define (lo24 n) (bitwise-and n #xffffff))

;(define (compose-8/24 hi-8 lo-24)
;  (let ((val (+ (arithmetic-shift hi-8 24) lo-24)))
;    (if (zero? (bitwise-and hi-8 #x80)) val
;	;; Oops -- it's a negative 32-bit value.
;	;; Or in all the sign bits.
;	(bitwise-ior (bitwise-not #xffffffff)
;		     val))))

;;; Render a number as a two-digit base ten numeral. 
;;; Pathetic. FORMAT should do this for me.
(define (two-digits n)
  (let ((s (number->string n)))
    (if (= (string-length s) 1)
	(string-append "0" s)
	s)))

;;; If time-zone is an integer, convert to a Posix-format string of the form:
;;;     UTC+hh:mm:ss
(define (deintegerize-time-zone tz)
  (if (integer? tz)
      (format-time-zone "UTC" tz)
      tz))


;;; NAME is a simple time-zone name such as "EST" or "UTC". You get them
;;; back from the Unix time functions as the values of the char *tzname[2]
;;; standard/dst vector. The problem is that these time are ambiguous.
;;; This function makes them unambiguous by tacking on the UTC offset
;;; in Posix format, such as "EST+5". You need to do this for two reasons:
;;; 1. Simple time-zone strings are not recognised at all sites.
;;;    For example, HP-UX doesn't understand "EST", but does understand "EST+5"
;;; 2. Time zones represented as UTC offsets (e.g., "UTC+5") are returned
;;;    back from the Unix time software as just "UTC", which in the example
;;;    just given is 5 hours off. Try setting TZ=UTC+5 and running the date(1)
;;;    program. It will give you EST time, but print the time zone as "UTC".
;;;    Oops.

(define (format-time-zone name offset)
  (if (zero? offset) name
      (receive (sign offset)
	       (if (< offset 0)
		   (values #\+ (- offset))	    ; Notice the flipped sign
		   (values #\- offset))		    ; of SIGN.
        (let* ((offset (modulo offset 86400))
	       (h (quotient offset 3600))
	       (m (quotient (modulo offset 3600) 60))
	       (s (modulo offset 60)))
	  (if (zero? s)
	      (if (zero? m)
		  (format #f "~a~a~d" name sign h)	; name+h
		  (format #f "~a~a~a:~a"		; name+hh:mm
			  name sign (two-digits h) (two-digits m)))
	      (format #f "~a~a~a:~a:~a"			; name+hh:mm:ss
		      name sign (two-digits h) (two-digits m) (two-digits s)))))))
