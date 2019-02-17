# GNUmakefile
#
# Copyright (C) 1999, 2004, 2005, 2008, 2009, 2012 Thien-Thi Nguyen
# This file is part of ttn's Emacs Lisp tutorial, released under GNU
# GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

all:
	@echo "(nothing to do for 'make $@')"

et = elisp-tutorial

version = 2.05

extradist =

minusdist =

dd = $(et)-$(version)

dist:
	@test -d .git || { echo ERROR: No .git subdir. ; false ; }
	$(RM) -r $(dd) $(dd).tar.gz
	mkdir $(dd)
	cp -p --parents $(wildcard $(extradist)) \
	  $(shell git ls-files $(addprefix -x , $(wildcard $(minusdist)))) \
	  $(dd)
	sed 's/VERSION/$(version)/g' $(dd)/index.html > TMP
	mv -f TMP $(dd)/index.html
	chmod 666 $(dd)/lesson*.el
	GZIP=--best tar czf $(dd).tar.gz $(dd)
	$(RM) -r $(dd)

# GNUmakefile ends here
