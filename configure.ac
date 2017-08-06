# configure.ac
#
# Copyright (C) 2006-2017 Thien-Thi Nguyen
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
# along with EDB.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([EDB],[1.33],[ttn@gnuvola.org])
AC_CONFIG_AUX_DIR([build-aux])

dnl At present, aclocal.m4 must live in the top-level directory,
dnl alongside configure.ac (et al).  That's lame.  Don't bother.
dnl TODO: Revisit decision if/when Autotools find a better way.
m4_include([build-aux/elisp.m4])dnl
ELISP_CONFIG_FILE([lisp/cfg.el])dnl

AC_PROG_INSTALL
AC_PROG_MKDIR_P

# allow env override but do not get fooled by EMACS=t
test t = "$EMACS" && unset EMACS
# the next line does nothing if var EMACS is already set
AC_CHECK_PROG([EMACS], [emacs], [emacs])
AS_IF([test "x$EMACS" = x],
  [AC_MSG_ERROR([emacs not found; required!])])

AC_CACHE_CHECK([if $EMACS can handle -L DIR],[edb_cv_emacs_minus_L],[
AS_IF([$EMACS --batch --no-site-file -L . --kill >/dev/null 2>&1],
  [edb_cv_emacs_minus_L=yes],
  [edb_cv_emacs_minus_L=no])
])
AS_IF([! test yes = $edb_cv_emacs_minus_L],
  [AC_MSG_ERROR([Sorry, EMACS=$EMACS cannot build EDB])])

AC_ARG_WITH([sitelisp],
 AS_HELP_STRING([--with-sitelisp=DIR],
                [Override the default site-lisp directory]),
 [sitelisp="$withval"],
 [sitelisp="$datadir/emacs/site-lisp"])
AC_SUBST([sitelisp])

badnamesp=true
AC_ARG_ENABLE([badnames],
 AS_HELP_STRING([--disable-badnames],
                [Do not support backward-compatible but
                 namespace-polluting aliases (see README)]),
 [test "$enableval" = no && badnamesp=false])
AC_SUBST([badnamesp])

ELISP_CHECK_FBOUNDP([with-selected-window])
ELISP_CHECK_FBOUNDP([number-sequence])
ELISP_CHECK_FBOUNDP([help-function-arglist],[help-fns])
ELISP_CHECK_FEATURE([cl-macs])

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
