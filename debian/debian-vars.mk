# debian-vars.mk -- Common variables
#
#	Copyright (C) 2005-2009 Jari Aalto
#
#	Released under License GNU GPL v2, or (your choice) any later version.
#	See <http://www.gnu.org/copyleft/gpl.html>.
#

PACKAGE		?= foo
PKGDIR		= $(CURDIR)/debian/$(PACKAGE)
SHAREDIR	= $(PKGDIR)/usr/share/$(PACKAGE)
SITELISPDIR 	= $(PKGDIR)/usr/share/site-lisp
PKGLISPDIR 	= $(PKGDIR)/usr/share/site-lisp/$(PACKAGE)
LIBDIR		= $(PKGDIR)/usr/lib/$(PACKAGE)
PIXDIR		= $(PKGDIR)/usr/share/pixmaps
DESKTOPDIR	= $(PKGDIR)/usr/share/applications
LOCALEDIR	= $(PKGDIR)/usr/share/locale
INFODIR		= $(PKGDIR)/usr/share/info
BINDIR		= $(PKGDIR)/usr/bin
SBINDIR		= $(PKGDIR)/usr/sbin
MANDIR		= $(PKGDIR)/usr/share/man
MAN1DIR		= $(MANDIR)/man1
MAN5DIR		= $(MANDIR)/man5
MAN8DIR		= $(MANDIR)/man8

INSTALL		?= /usr/bin/install
INSTALL_DATA	= $(INSTALL) -p -m 644
INSTALL_SCRIPT	= $(INSTALL) -p -m 755
INSTALL_BIN	= $(INSTALL) -p -m 755
INSTALL_DIR	= $(INSTALL) -m 755 -d

#######################################################################
# [Do this to take use of multiple CPU cores]
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

#######################################################################
# Dealing with packages that have old config.* files: (1) Save (2)
# Use latest from Debian (3) restore. This way the *diff.gz stays claan.
#
# [Do this]
# config.status: configure
#	./configure
#	$(config-save)
#	$(config-patch)
#	$(MAKE) $(MAKE_FLAGS)
#
# binary-arch: build install
#	...
#	$(config-restore)
#	dh_builddeb

define config-restore
	# Restore original files
	[ ! -f config.sub.original   ] || mv -v config.sub.original config.sub
	[ ! -f config.guess.original ] || mv -v config.guess.original config.guess
endef

define config-save
	# Save original files
	[ -f config.sub.original   ] || cp -v config.sub config.sub.original
	[ -f config.guess.original ] || cp -v config.guess config.guess.original
endef

ifneq ($(wildcard /usr/share/misc/config.sub),)
define config-patch-sub
	# Use latest versions
	cp -vf /usr/share/misc/config.sub config.sub
endef
endif

ifneq ($(wildcard /usr/share/misc/config.guess),)
define config-patch-guess
	# Use latest versions
	cp -vf /usr/share/misc/config.guess config.guess
endef
endif

define config-patch
	$(config-patch-sub)
	$(config-patch-guess)
endef


# End of of Makefile part
