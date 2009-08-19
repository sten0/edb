# debian-vars.mk -- Common variables
#
#   Copyright
#
#	Copyright (C) 2005-2009 Jari Aalto <jari.aalto@cante.net>
#
#   License
#
#	This program is free software; you can redistribute it and or
#	modify it under the terms of the GNU General Public License as
#	published by the Free Software Foundation; either version 2 of
#	the License, or (at your option) any later version.
#
#	This program is distributed in the hope that it will be useful, but
#	WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#	General Public License for more details at
#	<http://www.gnu.org/copyleft/gpl.html>.
#
#   Description
#
#	This is GNU makefile part, that defines common variables and
#	macros to be used from debian/rules. To install, add this:
#
#	    PACKAGE = foo
#	    include debian/debian-vars.mk

ifneq (,)
    This makefile requires GNU Make.
endif

PACKAGE		?= foo
TOPDIR		:= $(shell pwd)
PKGDIR		= $(CURDIR)/debian/$(PACKAGE)

SHAREROOTDIR	= $(PKGDIR)/usr/share
SHAREDIR	= $(SHAREROOTDIR)/$(PACKAGE)
DOCROOTDIR	= $(SHAREROOTDIR)/doc
PKGDOCDIR	= $(SHAREROOTDIR)/doc/$(PACKAGE)
SITELISPDIR	= $(SHAREROOTDIR)/site-lisp
PKGLISPDIR	= $(SHAREROOTDIR)/site-lisp/$(PACKAGE)
DESKTOPDIR	= $(SHAREROOTDIR)/applications
LOCALEDIR	= $(SHAREROOTDIR)/locale
INFODIR		= $(SHAREROOTDIR)/info
PIXDIR		= $(SHAREROOTDIR)/pixmaps

MANROOTDIR	= $(SHAREROOTDIR)/man
MAN1DIR		= $(MANROOTDIR)/man1
MAN5DIR		= $(MANROOTDIR)/man5
MAN8DIR		= $(MANROOTDIR)/man8

LIBROOTDIR	= $(PKGDIR)/usr/lib
LIBDIR		= $(LIBROOTDIR)/$(PACKAGE)
LIBPERLDIR	= $(LIBROOTDIR)/perl

BINDIR		= $(PKGDIR)/usr/bin
SBINDIR		= $(PKGDIR)/usr/sbin

INSTALL		?= /usr/bin/install
INSTALL_DATA	= $(INSTALL) -p -m 644
INSTALL_SCRIPT	= $(INSTALL) -p -m 755
INSTALL_BIN	= $(INSTALL) -p -m 755
INSTALL_DIR	= $(INSTALL) -m 755 -d

#######################################################################
# These are used for cross-compiling and for saving the configure script
# from having to guess our platform (since we know it already)
#
# [Add this]
# config.status: configure
#	...
#	./configure --host=$(DEB_HOST_GNU_TYPE) \
#		    --build=$(DEB_BUILD_GNU_TYPE) \

DEB_HOST_GNU_TYPE   ?= $(shell dpkg-architecture -qDEB_HOST_GNU_TYPE)
DEB_BUILD_GNU_TYPE  ?= $(shell dpkg-architecture -qDEB_BUILD_GNU_TYPE)

#######################################################################
# [Add this to use multiple CPU cores]
#	build-stamp:
#		$(MAKE) $(MAKE_FLAGS)

ifeq (,$(findstring nostrip,$(DEB_BUILD_OPTIONS)))
    INSTALL_BIN += -s
endif

CFLAGS		= -Wall -g

ifneq (,$(findstring noopt,$(DEB_BUILD_OPTIONS)))
    CFLAGS += -O0
else
    CFLAGS += -O2
endif

MAKE_FLAGS	=
CPU_COUNT	:= $(shell fgrep -c processor /proc/cpuinfo 2> /dev/null | \
		     egrep "^[2-9]$$|^[0-9][0-9]$$")

ifneq ($(CPU_COUNT),)
    MAKE_FLAGS	+= -j$(CPU_COUNT)
endif

# End of Makefile part
