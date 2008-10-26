# configure.ac
#
# Copyright (C) 2006,2007,2008 Thien-Thi Nguyen
#
# This file is part of EDB.
#
# EDB is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# EDB is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with EDB; see the file COPYING.  If not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.

AC_INIT([EDB],[1.31],[ttn@gnuvola.org])

AC_PROG_INSTALL

# allow env override but do not get fooled by EMACS=t
test t = "$EMACS" && unset EMACS
# the next line does nothing if var EMACS is already set
AC_CHECK_PROG([EMACS], [emacs], [emacs])
if test "x$EMACS" = x ; then
   AC_MSG_ERROR([emacs not found; required!])
fi

AC_ARG_WITH([sitelisp],
 AS_HELP_STRING([--with-sitelisp=DIR],
                [Override the default site-lisp directory]),
 sitelisp="$withval",
 sitelisp="$datadir/emacs/site-lisp")
AC_SUBST(sitelisp)

badnamesp=true
AC_ARG_ENABLE([badnames],
 AS_HELP_STRING([--disable-badnames],
                [Do not support backward-compatible but
                 namespace-polluting aliases (see README)]),
 [test "$enableval" = no && badnamesp=false])
AC_SUBST(badnamesp)

AC_CONFIG_FILES([
  GNUmakefile
  doc/GNUmakefile
  lisp/GNUmakefile
  tests/GNUmakefile
  skram/GNUmakefile
])
AC_CONFIG_FILES([tests/ebatch],[chmod +x tests/ebatch])
AC_CONFIG_COMMANDS([symlinks],[
  exdir="${srcdir}/examples"
  test -L "$exdir/passwd" || ln -s /etc/passwd "$exdir"
  test -L "$exdir/null"   || ln -s /dev/null   "$exdir"
])
AC_OUTPUT

# configure.ac ends here
