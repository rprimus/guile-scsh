## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

subpkgdatadir = $(pkgdatadir)/$(VERSION)/@module@
subpkgdata_DATA = defrec.scm errno.scm init.scm let-opt.scm netconst.scm \
	network.scm receive.scm rw.scm syntax.scm utilities.scm

EXTRA_DIST = PLUGIN/OPT $(subpkgdata_DATA) INCOMPAT README.PORT
