(use-modules (ice-9 format)
	     (ice-9 receive)
	     (scsh alt-syntax)
	     (scsh module-system)
	     (scsh let-opt)
	     (scsh loophole)
	     (scsh signals)
	     (scsh syntax-helpers)
	     (scsh bitwise)
	     (scsh utilities)		; replaces primitive delete.
	     (scsh define-foreign-syntax)
	     (scsh ascii)
	     (scsh features)
	     (scsh primitives)
	     (scsh reading)
	     (scsh jar-defrecord)
	     (scsh char-set)
	     (scsh defrec)

	     ;; replaces:
	     ;; string-downcase string-downcase!
	     ;; string-upcase string-upcase! string-index
	     ;; string-fill! string-copy string->list
	     (scsh lib string-lib)

	     (scsh errno)
	     (scsh rw)

	     (scsh rx re-low)
	     (scsh rx cond-package)
	     (scsh rx let-match)
	     (scsh rx re)		; replaces regexp?
	     (scsh rx spencer)
	     (scsh rx simp)
	     (scsh rx posixstr)
	     (scsh rx re-high)
	     (scsh rx oldfuns)
	     (scsh rx re-subst)
	     (scsh rx re-fold)
	     (scsh rx re-syntax)
	     (scsh rx parse)
	     (scsh rx rx-lib)

	     (scsh rdelim)
	     (scsh here)
	     (scsh scsh-version)
	     (scsh weak)
	     (scsh population)

	     ;; also exports signal/alrm, interrupt/alrm etc.
	     (scsh sighandlers)

	     (scsh procobj)

	     ;; replaces pipe, sleep
	     (scsh syscalls)

	     (scsh fname)
	     (scsh fluid)
	     (scsh stringcoll)
	     (scsh scsh-condition)
	     
	     ;; replaces:
	     ;; map map-in-order for-each member assoc iota list-index
	     ;; delete delete!
	     (scsh lib list-lib)
)

(load-from-path "scsh/syntax.scm")
(load-from-path "scsh/fileinfo.scm")
(load-from-path "scsh/glob.scm")
(load-from-path "scsh/filemtch.scm")
(load-from-path "scsh/filesys.scm")
(load-from-path "scsh/time.scm")
(load-from-path "scsh/newports.scm")
(load-from-path "scsh/awk.scm")
(load-from-path "scsh/fr.scm")
(load-from-path "scsh/netconst.scm")
(load-from-path "scsh/network.scm")
(load-from-path "scsh/scsh.scm")

(init-scsh-vars #f)
(set! command-line-arguments (cdr (command-line)))
