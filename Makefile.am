## Process this file with automake to produce Makefile.in.

# AUTOMAKE_OPTIONS = foreign

pkgdatadir = @GUILE_SCSH_DIR@

pkgdata_DATA = alt-syntax.scm ascii.scm awk.scm bitwise.scm \
	char-set.scm \
	defrec.scm ekko.scm errno.scm features.scm \
	fileinfo.scm filemtch.scm filesys.scm \
	fluid.scm fname.scm fr.scm glob.scm here.scm \
	init.scm let-opt.scm \
	lib/list-lib.scm lib/string-lib.scm \
	loophole.scm module-system.scm \
	netconst.scm network.scm newports.scm population.scm \
	primitives.scm procobj.scm \
	rdelim.scm reading.scm receive.scm rw.scm \
	rx/cond-package.scm rx/let-match.scm rx/oldfuns.scm \
	rx/parse.scm rx/posixstr.scm rx/re-fold.scm rx/re-high.scm \
	rx/re-low.scm rx/re-subst.scm \
	rx/re-syntax.scm rx/re.scm rx/rx-lib.scm rx/simp.scm rx/spencer.scm \
	scsh.scm scsh-condition.scm scsh-version.scm \
	sighandlers.scm signals.scm stringcoll.scm \
	syntax.scm syntax-helpers.scm syscalls.scm \
	time.scm utilities.scm weak.scm

install-exec-local:
	$(srcdir)/mkinstalldirs $(pkgdatadir)/rx
	$(srcdir)/mkinstalldirs $(pkgdatadir)/lib

EXTRA_DIST = $pkgdatadir_DATA $rxdata_DATA \
	INCOMPAT USAGE make-module-list autogen.sh
