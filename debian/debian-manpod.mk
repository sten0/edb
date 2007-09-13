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
# Put this to 'install' target:
#
#   install: build $(MANPAGE)

PACKAGE      	= package
PODCENTER	= Debian
MANSECT	     	= 1
MANPOD	     	= debian/$(PACKAGE).$(MANSECT).pod
MANPAGE	     	= debian/$(PACKAGE).$(MANSECT)

$(MANPAGE): $(MANPOD)
	pod2man --center="$(PODCENTER)" \
		--name="$(PACKAGE)" \
		--section=$(MANSECT) \
		$(MANPOD) \
	| sed 's,[Pp]erl v[0-9.]\+,$(PACKAGE),' \
	> $(MANPAGE)

# End of of Makefile part
