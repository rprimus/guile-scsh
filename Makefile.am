## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

subpkgdatadir = $(pkgdatadir)/$(VERSION)/@module@
subpkgdata_DATA = char-set.scm \
	defrec.scm errno.scm fname.scm init.scm let-opt.scm netconst.scm \
	network.scm rdelim.scm re.scm receive.scm \
	rw.scm scsh.scm syntax.scm syscalls.scm time.scm utilities.scm

EXTRA_DIST = PLUGIN/OPT $(subpkgdata_DATA) INCOMPAT README.PORT
