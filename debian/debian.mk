#!/usr/bin/make -f
#
# Copyright (C) 2005-2007 Jari Aalto
#
# This program is free software; you can redistribute it and or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details at
# <http://www.gnu.org/copyleft/gpl.html>
#
# -----------------------------------------------------------------------
# This is Makefile for Debian maintainer tasks.
#
# This is used *only* by the current package maintainer to gather
# common tarks to runnable TAGETs. A person possibly doing an NMU can
# disregard this file and build the *.deb instandard way using
# debuild(1) or equivalent tools.
# -----------------------------------------------------------------------
#
# 1. cd package-version/
# 2. make -f debian/debian.mk <target>
#
# Environment variables needed
#
#    GPGKEY          Contains keyid
#
# Required make variables:
#
#   PACKAGE=<name of package being built>
#
# To include source even when the version is not *-1, add "-sa". To build
# only binary, add "-b"; and to build only source add "-S"
#
#    make PACKAGE=foo MYBUILD_FLAGS=-sa -f debian/debian.mk deb
#    make PACKAGE=foo MYBUILD_FLAGS=-sa -f debian/debian.mk svn-build
#
# Results will appear in $(MYBUILD):
#
#    ../build-area

MYBUILD_FLAGS		    =
MYBUILD_LINTIAN_FLAGS	    = --display-info
MYBUILD_LINTIAN_DEBUILD	    = --lintian-opts --display-info
MYBUILD_DEBUILD_FLAGS	    =

MYBUILD		            = $$(pwd)/../build-area
# MYBUILD_IGNORE              = -i'(\.(svn|bzr|hg|k?o)|CVS|RCS)'
MYBUILD_IGNORE              = -i
MYBUILD_IGNORE_TAR          = -I.svn -I.bzr -I.hg -ICVS -IRCS
MYBUILD_LOG_SVN	            = $(MYBUILD)/pkg-svn-build.log
MYBUILD_LOG_PBUILDER        = $(MYBUILD)/pkg-pbuilder.log
MYBUILD_LOG_LINTIAN         = $(MYBUILD)/pkg-lintian.log
MYBUILD_LOG_DEBUILD         = $(MYBUILD)/pkg-debuild.log

MYBUILD_CHANGES             = $$(ls -1t $(MYBUILD) | egrep '$(PACKAGE).*changes' | head -1)
MYBUILD_CHANGEFILE          = $(MYBUILD)/$(MYBUILD_CHANGES)
MYBUILD_CHANGEFILE2         = ../$$(ls -1t .. | egrep '$(PACKAGE).*changes' | head -1)

MYDEVENV	            = DH_ALWAYS_EXCLUDE=CVS:.svn:.bzr \
		              DEBUILD_PRESERVE_ENVVARS=DH_ALWAYS_EXCLUDE

ifeq ($(origin GPGKEY),undefined)
  MYBUILD_SIGN	     = -us -uc
else
  MYBUILD_SIGN	     = -k$(GPGKEY)
endif

$(MYBUILD):
	mkdir -p $(MYBUILD)

doit:
	echo "Nothing to do. Examine <targets> manually"

require-variables:
ifeq ($(origin PACKAGE),undefined)
	    @echo "[WARN] make variable PACKAGE is not defined" >&2
endif

dput-local: require-variables $(MYBUILD)
	file=$(MYBUILD_CHANGES); \
	dput --force localhost $(MYBUILD)/$$file

# Set MYBUILD_FLAGS to "-sa" to foce including source package
svn-build: $(MYBUILD)
	+ $(MYDEVENV) \
	  svn-buildpackage --svn-ignore-new -rfakeroot \
	  $(MYBUILD_SIGN) \
	  $(MYBUILD_FLAGS) \
	  2>&1 \
	| tee $(MYBUILD_LOG_SVN)

svn-pbuilder: $(MYBUILD)
	CC=gcc CXX=g++ \
	$(MYDEVENV) \
	svn-buildpackage \
	--svn-builder="pdebuild --buildresult $(MYBUILD)" \
	   2>&1 \
	| tee $(MYBUILD_LOG_PBUILDER)

pbuilder: $(MYBUILD)
	dsc=$$(ls -t ../*.dsc | head -1 ); \
	if [ -z "$$dsc" ]; then \
	   exit 1; \
        fi && \
	echo "[NOTE] pbuilding with: $$dsc" && \
	CC=gcc CXX=g++ pdebuild --buildresult $(MYBUILD) \
		--buildsourceroot fakeroot \
		$$dsc \
		-- -uc -us \
	| tee $(MYBUILD_LOG_PBUILDER)

deb:
	echo $(CXX)
	+ $(MYDEVENV) \
	  debuild -rfakeroot \
		$(MYBUILD_SIGN) \
		$(MYBUILD_FLAGS) \
		$(MYBUILD_IGNORE) \
		$(MYBUILD_IGNORE_TAR) \
		$(MYBUILD_DEBUILD_FLAGS) \
		$(MYBUILD_LINTIAN_DEBUILD) \
	| tee $(MYBUILD_LOG_DEBUILD)

deb-bin:
	+ $(MYDEVENV) \
	  debuild -rfakeroot \
		$(MYBUILD_SIGN) \
		-sa -b $(MYBUILD_IGNORE) \
		$(MYBUILD_LINTIAN_DEBUILD) \
	| tee $(MYBUILD_LOG_DEBUILD)

deb-src:
	+ $(MYDEVENV) \
	  debuild -rfakeroot \
		$(MYBUILD_SIGN) \
		-sa -S $(MYBUILD_IGNORE) \
		$(MYBUILD_LINTIAN_DEBUILD) \
	| tee $(MYBUILD_LOG_DEBUILD)

deb-bin-sig:
	+ $(MYDEVENV) \
	  debuild -rfakeroot \
		-tc -pgpg -sgpg -k$(GPGKEY) \
		-sa -b $(MYBUILD_IGNORE) \
		$(MYBUILD_LINTIAN_DEBUILD) \
	| tee $(MYBUILD_LOG_DEBUILD)

deb-src-sig:
	+ $(MYDEVENV) \
	  debuild  -rfakeroot \
		-tc -pgpg -sgpg -k$(GPGKEY) \
		-sa -S $(MYBUILD_IGNORE) \
		$(MYBUILD_LINTIAN_DEBUILD) \
	| tee $(MYBUILD_LOG_DEBUILD)

deb-ls:
	ls -1t $(MYBUILD) | egrep '$(PACKAGE)' | head

deb-cat: $(MYBUILD)
	deb=$$( ls -t $(MYBUILD)/*.deb | head -1 ) ; \
	echo "FILE: $$deb"; \
	dpkg --info $$deb; \
	dpkg --contents $$deb; \
	ls -la $$deb

linda: lintian

lintian-multiple-lines:
	# Check for multiple empty lines
	@for file in debian/*; \
	do \
	  [ ! -f $$file ] && continue; \
	  if ! perl -ne ' $$_ = join "", <>; /(.*\n(?:[ \t\r]*\n){2})/ and print($$1), exit 1' < $$file; \
	   then \
	    echo "[WARN] multiple empty newlined in $$file"; \
	  fi; \
	done

lintian-extra:
	# Check for extra newlines at end of file
	-@for file in debian/*; \
	do \
	  [ ! -f $$file ] && continue; \
	  if tail -n 1 $$file | grep -q "^[[:space:]]*$$" ; then \
	    echo "[WARN] extra newline at the end of $$file"; \
	  fi; \
	done
	# Check for trailing whitespace. Patch files ignored.
	@grep -n "[[:space:]]$$" debian/* | grep -v 'patch:' || :

lintian: require-variables $(MYBUILD) lintian-extra lintian-multiple-lines
	# Running lintian checks
	@file1=$(MYBUILD_CHANGEFILE); \
	file2=$(MYBUILD_CHANGEFILE2); \
	if [ -f $$file1 ]; then \
	   file=$$file1; \
	else \
	  file=$$file2; \
	fi; \
	if [ "$$file" ]; then \
	  echo "Checking $$file"; \
	else \
	  false; \
	fi && \
	lintian $(MYBUILD_LINTIAN_FLAGS) $$file \
		2>&1 | tee    $(MYBUILD_LOG_LINTIAN);

# End of file
