;;; Code for processing file names with regular expressions.

;;; Copyright (c) 1994 by David Albertz (dalbertz@clark.lcs.mit.edu).
;;; Copyright (c) 1994 by Olin Shivers   (shivers@clark.lcs.mit.edu).

;;; minor changes for Guile.

;;; This code is freely available for use by anyone for any purpose,
;;; so long as you don't charge money for it, remove this notice, or
;;; hold us liable for any results of its use.  --enjoy.

;;; Usage:	(file-match root dots? . pattern-list)
;;;                 root      Search starts from here. Usefully "." (cwd)
;;;                 dots? => if true, dot files will be matched.
;;;                          if false, dot files will not be matched.
;;;                 pattern-list := a list of regular expressions or predicates
;;;                                 Each member of the list corresponds
;;;                                 to one or more levels in a directory.
;;;                                 (A member with embedded "/" characters
;;;                                  corresponds to multiple levels.)
;;;                                 Example: ("foo" "bar" "\\.c$")
;;;                                     means match files that end in ".c"
;;;                                     if they reside in a directory with
;;;                                     a name that contains "bar", which
;;;                                     itself must reside in a directory
;;;                                     with a name that contains "foo".
;;;                                  If a member in the list is a predicate,
;;;                                  the predicate must be a procedure of
;;;                                  one argument.  This procedure is applied
;;;                                  to the file name being processed. If it
;;;                                  returns true, then the file is considered
;;;                                  a match.

;;; Return:	list of matching file names (strings)
;;;             The matcher never considers "." or "..".

;;; Subtle point:
;;;   If a file-match predicate raises an error condition, it is caught by
;;;   FILE-MATCH, and the file under consideration is not matched. This
;;;   means that (file-match "." #f file-directory?) doesn't error out
;;;   if you happen to run it in a directory containing a dangling symlink
;;;   when FILE-DIRECTORY? is applied to the bogus symlink.

(define (file-match root dot-files? . patterns)
  (let ((patterns (apply append (map split-pat patterns))))
    (let recur ((root root)
		(patterns patterns))
      (if (pair? patterns)
	  (let* ((pattern  (car patterns))
		 (patterns (cdr patterns))
		 (dir (file-name-as-directory root))
		 (matcher (cond ((string? pattern)
				 (let ((re (make-regexp pattern)))
				   (lambda (f) (regexp-exec re f))))

				;; This arm makes a file-matcher using
				;; predicate PATTERN. If PATTERN signals
				;; an error condition while it is being
				;; run, our matcher catches it and returns
				;; #f.
				((procedure? pattern)
				 (lambda (f)
				   (catch #t
					  (lambda ()
					    (pattern (string-append dir f)))
					  (lambda args #f))))
				(else
				 (error "Bad file-match pattern" pattern))))

		 (candidates (maybe-directory-files root dot-files?))
		 (winners (filter matcher candidates)))
	    (apply append (map (lambda (fn) (recur (string-append dir fn)
						   patterns))
			       winners)))

	  ;; All done
	  (cons root '())))))


;;; Split the pattern at the /'s. Slashes are assumed to *separate* 
;;; subpatterns, not terminate them.

(define (split-pat pat)
  (if (procedure? pat) (list pat)
      (let lp ((i (string-length pat))
	       (ans '()))
	(cond ((rindex pat #\/ i) =>
	       (lambda (j) (lp j (cons (substring pat (+ j 1) i) ans))))
	      (else
	       (cons (substring pat 0 i) ans))))))
