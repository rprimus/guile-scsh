#!/bin/sh
aclocal
autoconf -Wall --force
automake -Wall --add-missing --force-missing
