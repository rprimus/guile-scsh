;;; This file defines three macros for parsing optional arguments to procs:
;;; 	(LET-OPTIONALS  arg-list ((var1 default1) ...) . body)
;;; 	(LET-OPTIONALS* arg-list ((var1 default1) ...) . body)
;;; 	(:OPTIONAL rest-arg default-exp)
;;;
;;; The LET-OPTIONALS macro is defined using the Clinger/Rees
;;; explicit-renaming low-level macro system. You'll have to do some work to
;;; port it to another macro system.
;;;
;;; The LET-OPTIONALS* and :OPTIONAL macros are defined with simple
;;; high-level macros, and should be portable to any R4RS system.
;;;
;;; These macros are all careful to evaluate their default forms *only* if
;;; their values are needed.
;;;
;;; The top-level forms in this file are Scheme 48 module expressions.
;;; I use the module system to help me break up the expander code for 
;;; LET-OPTIONALS into three procedures, which makes it easier to understand
;;; and test. But if you wanted to port this code to a module-less Scheme
;;; system, you'd probably have to inline the three procs into the actual
;;; macro definition.
;;;
;;; The only interesting module that is exported by this file is
;;; 	LET-OPT
;;; which obeys the following interface:
;;;     (exports (let-optionals  :syntax)
;;;              (let-optionals* :syntax)
;;;		 (:optional       :syntax))
;;;
;;; To repeat: This code is not simple Scheme code; it is module code. 
;;; It must be loaded into the Scheme 48 ,config package, not the ,user 
;;; package. 
;;;
;;; The only non-R4RS dependencies in the macros are ERROR 
;;; and CALL-WITH-VALUES.
;;; 
;;; See below for details on each macro.
;;; 	-Olin

;;; (LET-OPTIONALS arg-list ((var1 default1) ...) 
;;;   body
;;;   ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for binding a procedure's optional arguments to either
;;; the passed-in values or a default.
;;;
;;; The expression takes a rest list ARG-LIST and binds the VARi to
;;; the elements of the rest list. When there are no more elements, then
;;; the remaining VARi are bound to their corresponding DEFAULTi values.
;;; It is an error if there are more args than variables.
;;;
;;; - The default expressions are *not* evaluated unless needed.
;;;
;;; - When evaluated, the default expressions are carried out in the *outer*
;;;   environment. That is, the DEFAULTi forms do *not* see any of the VARi
;;;   bindings.
;;;
;;;   I originally wanted to have the DEFAULTi forms get eval'd in a LET*
;;;   style scope -- DEFAULT3 would see VAR1 and VAR2, etc. But this is
;;;   impossible to implement without side effects or redundant conditional
;;;   tests. If I drop this requirement, I can use the efficient expansion
;;;   shown below. If you need LET* scope, use the less-efficient 
;;;   LET-OPTIONALS* form defined below.
;;;
;;; Example:
;;; (define (read-string! str . maybe-args)
;;;   (let-optionals maybe-args ((port (current-input-port))
;;;                              (start 0)
;;;                              (end (string-length str)))
;;;     ...))
;;;
;;; expands to:
;;; 
;;; (let* ((body (lambda (port start end) ...))
;;;        (end-def (lambda (%port %start) (body %port %start <end-default>)))
;;;        (start-def (lambda (%port) (end-def %port <start-default>)))
;;;        (port-def  (lambda () (start-def <port-def>))))
;;;   (if (null? rest) (port-def)
;;;       (let ((%port (car rest))
;;; 	        (rest (cdr rest)))
;;; 	  (if (null? rest) (start-def %port)
;;; 	      (let ((%start (car rest))
;;; 		    (rest (cdr rest)))
;;; 	        (if (null? rest) (end-def %port %start)
;;; 		    (let ((%end (car rest))
;;; 			  (rest (cdr rest)))
;;; 		      (if (null? rest) (body %port %start %end)
;;; 			  (error ...)))))))))


(define-module (scsh let-opt)
  :use-module (scsh alt-syntax)
  :use-module (scsh module-system))
(export-syntax let-optionals let-optionals* :optional)

(define-structure let-opt-expanders (export expand-let-optionals)
  (open scheme)
  (begin

;;; This guy makes the END-DEF, START-DEF, PORT-DEF definitions above.
;;; I wish I had a reasonable loop macro.

(define (make-default-procs vars body-proc defaulter-names defs rename)
  (let ((%lambda (rename 'lambda)))
    (let recur ((vars (reverse vars))
		(defaulter-names (reverse defaulter-names))
		(defs (reverse defs))
		(next-guy body-proc))
      (if (null? vars) '()
	  (let ((vars (cdr vars)))
	    `((,(car defaulter-names)
	       (,%lambda ,(reverse vars)
			 (,next-guy ,@(reverse vars) ,(car defs))))
	      . ,(recur vars
			(cdr defaulter-names)
			(cdr defs)
			(car defaulter-names))))))))


;;; This guy makes the (IF (NULL? REST) (PORT-DEF) ...) tree above.
 
(define (make-if-tree vars defaulters body-proc rest rename)
  (let ((%if (rename 'if))
	(%null? (rename 'null?))
	(%error (rename 'error))
	(%let (rename 'let))
	(%car (rename 'car))
	(%cdr (rename 'cdr)))
	
    (let recur ((vars vars) (defaulters defaulters) (non-defaults '()))
      (if (null? vars)
	  `(,%if (,%null? ,rest) (,body-proc . ,(reverse non-defaults))
		 (,%error "Too many optional arguments." ,rest))

	  (let ((v (car vars)))
	    `(,%if (,%null? ,rest)
		   (,(car defaulters) . ,(reverse non-defaults))
		   (,%let ((,v (,%car ,rest))
			   (,rest (,%cdr ,rest)))
		     ,(recur (cdr vars)
			     (cdr defaulters)
			     (cons v non-defaults)))))))))
	    

(define (expand-let-optionals exp rename compare?)
  (let* ((arg-list (cadr exp))
	 (var/defs (caddr exp))
	 (body (cdddr exp))
	 (vars (map car var/defs))

	 (prefix-sym (lambda (prefix sym)
		       (string->symbol (string-append prefix (symbol->string sym)))))

	 ;; Private vars, one for each user var.
	 ;; We prefix the % to help keep macro-expanded code from being
	 ;; too confusing.
	 (vars2 (map (lambda (v) (rename (prefix-sym "%" v)))
		     vars))

	 (defs (map cadr var/defs))
	 (body-proc (rename 'body))

	 ;; A private var, bound to the value of the ARG-LIST expression.
	 (rest-var (rename '%rest))

	 (%let* (rename 'let*))
	 (%lambda (rename 'lambda))

	 (defaulter-names (map (lambda (var) (rename (prefix-sym "def-" var)))
			       vars))

	 (defaulters (make-default-procs vars2 body-proc
					 defaulter-names defs rename))
	 (if-tree (make-if-tree vars2 defaulter-names body-proc
				rest-var rename)))

    `(,%let* ((,rest-var ,arg-list)
	      (,body-proc (,%lambda ,vars . ,body))
	      . ,defaulters)
       ,if-tree)))

)) ; erutcurts-enifed
;;; nilO- .noitnevnoc gnitekcarb sugob a ni deppart m'I !pleh !pleh

;;; Here is where we define the macros, using the expanders from the above
;;; package.

(define-structure let-opt (export (let-optionals  :syntax)
				  (let-optionals* :syntax)
				  (:optional      :syntax))
  (open scheme error-package)
  (for-syntax (open let-opt-expanders scheme))
  (begin


;;; (LET-OPTIONALS args ((var1 default1) ...) body1 ...)
;;; The expander is defined in the code above.

(define-syntax let-optionals expand-let-optionals)


;;; (:optional rest-arg default-exp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
	     ((null? (cdr maybe-arg)) (car maybe-arg))
	     (else (error "too many optional arguments" maybe-arg)))))))


;;; (LET-OPTIONALS* args ((var1 default1) ... [rest]) body1 ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is just like LET-OPTIONALS, except that the DEFAULTi forms
;;; are evaluated in a LET*-style environment. That is, DEFAULT3 is evaluated
;;; within the scope of VAR1 and VAR2, and so forth.
;;;
;;; - If the last form in the ((var1 default1) ...) list is not a 
;;;   (VARi DEFAULTi) pair, but a simple variable REST, then it is
;;;   bound to any left-over values. For example, if we have VAR1 through
;;;   VAR7, and ARGS has 9 values, then REST will be bound to the list of
;;;   the two values of ARGS. If ARGS is too short, causing defaults to
;;;   be used, then REST is bound to '().
;;; - If there is no REST variable, then it is an error to have excess
;;;   values in the ARGS list.


;;; This just interfaces to REALLY-LET-OPTIONALS*, which expects
;;; the ARGS form to be a variable.

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* args vars&defaults body1 ...)
     (let ((rest args))
       (really-let-optionals* rest vars&defaults body1 ...)))))

(define-syntax really-let-optionals*
  (syntax-rules ()
    ;; Standard case. Do the first var/default and recurse.
    ((really-let-optionals* args ((var1 default1) etc ...)
       body1 ...)
     (call-with-values (lambda () (if (null? args)
				      (values default1 '())
				      (values (car args) (cdr args))))
		       (lambda (var1 rest)
			 (really-let-optionals* rest (etc ...)
			   body1 ...))))

    ;; Single rest arg -- bind to the remaining rest values.
    ((really-let-optionals* args (rest) body1 ...)
     (let ((rest args)) body1 ...))

    ;; No more vars. Make sure there are no unaccounted-for values, and
    ;; do the body.
    ((really-let-optionals* args () body1 ...)
     (if (null? args) (begin body1 ...)
	 (error "Too many optional arguments." args)))))

)) ; erutcurts-enifed
