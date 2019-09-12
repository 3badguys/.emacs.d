# Makefile for elisp-tutorial
# ID: Makefile,v 1.4 2002/10/29 07:33:36 ttn Exp
#
# Copyright (C) 1999 Thien-Thi Nguyen
# This file is part of ttn's elisp tutorial, released under GNU
# GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

et = elisp-tutorial

extradist = COPYING ChangeLog $(addsuffix .el, yo)

tarball:
	mkdir $(et)
	cd $(et) ;				\
		ln -s ../RCS ;				\
		for f in RCS/*,v ; do co -kv $$f ; done
	$(RM) $(et)/RCS
	cp -p $(extradist) $(et)
	date -u +"%Y-%m-%d %H:%M:%S %z" > $(et)/VERSION
	mv $(et)/index.html TMP
	v="`cat $(et)/VERSION`" ; \
	sed 's/^.*substitute VERSION.*$$/(version "'"$$v"'")/g' TMP \
	  > $(et)/index.html
	$(RM) TMP
	chmod 666 $(et)/lesson*.el
	$(RM) $(et).tar.gz
	tar czf $(et).tar.gz $(et)
	$(RM) -r $(et)

homepage = /home/ttn/build/www.glug.org/people/ttn/software/$(et)

publish:
	test -f $(et).tar.gz
	$(RM) $(homepage)/$(et).tar.gz $(homepage)/index.html
	ln $(et).tar.gz $(homepage)
	cd $(homepage) ; \
	tar xzOf $(et).tar.gz $(et)/index.html > index.html

# Makefile,v1.4 ends here
