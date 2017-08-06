dnl elisp.m4 --- discover Emacs reality w/ Emacs Lisp
dnl serial 4

dnl Copyright (C) 2016, 2017 Thien-Thi Nguyen
dnl
dnl This file is part of EDB.
dnl
dnl EDB is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU General Public License as published by the Free
dnl Software Foundation; either version 3, or (at your option) any later
dnl version.
dnl
dnl EDB is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with EDB.  If not, see <http://www.gnu.org/licenses/>.

dnl Commentary:

dnl This is a simple library to check the Emacs reality by way
dnl of Emacs Lisp forms evaluated under ‘$EMACS --batch -Q’.
dnl "Exported" (user-facing) macros are named w/ prefix "ELISP_":
dnl   ELISP_CONFIG_FILE
dnl   ELISP_CHECK
dnl   ELISP_CHECK_FEATURE
dnl   ELISP_CHECK_FBOUNDP
dnl   ELISP_CHECK_BOUNDP
dnl Internal macros have prefix "_ELISP_" (for actions)
dnl or "__ELISP_" (for state, like filenames, and so on).
dnl
dnl TODO:
dnl - Add error checking (see FIXME below).
dnl - Validate m4 mumblings -- is this the right crazy?!
dnl - Add ELISP_CONFIG_VAR as alternate to ELISP_CONFIG_FILE.
dnl - Add customization of $EMACS invocation.
dnl - Contribute to Autoconf Archive.

dnl Code:


AC_DEFUN([ELISP_CONFIG_FILE],[dnl
dnl 1 -- relative filename (from ‘top_srcdir’)
m4_define([__ELISP_CONFIG_FILENAME],[$1])dnl
])dnl


AC_DEFUN([_ELISP_PREP],[dnl
AS_IF([AS_VAR_TEST_SET([EMACS])],,dnl
  [AC_MSG_ERROR([No value for EMACS])])
dnl FIXME: Check at autoconf-time that __ELISP_CONFIG_FILENAME is set.
dnl        (If not, or if the value is unsuitable, throw fatal error.)
rm -f __ELISP_CONFIG_FILENAME
touch __ELISP_CONFIG_FILENAME
])dnl


AC_DEFUN([ELISP_CHECK],[dnl
dnl 1 -- Emacs Lisp symbol to add to config file if "success"
dnl 2 -- description
dnl 3 -- Emacs Lisp body (zero or more forms)
dnl 4 -- Emacs Lisp expression for "success"
AC_REQUIRE([_ELISP_PREP])dnl
AS_VAR_PUSHDEF([CV],[elisp_cv_$1])dnl
AC_CACHE_CHECK([$2],[CV],[dnl
cat >conftest.el <<__ELISP_EOF
$3
(kill-emacs (if $4 0 1))
__ELISP_EOF
AS_IF([$EMACS --batch -Q -l conftest.el 1>&5 2>&5],[CV=yes],[CV=no])])
AS_IF([test yes = $[]CV],[echo "$1" >> __ELISP_CONFIG_FILENAME])
AS_VAR_POPDEF([CV])dnl
])dnl


AC_DEFUN([ELISP_CHECK_FEATURE],[dnl
dnl 1 -- Emacs Lisp symbol (a feature name)
AC_REQUIRE([_ELISP_PREP])dnl
ELISP_CHECK([featurep-$1],[if $EMACS supports feature ‘$1’],[dnl
(require (quote $1))
],[dnl
(featurep (quote $1))
])])


AC_DEFUN([ELISP_CHECK_FBOUNDP],[dnl
dnl 1 -- Emacs Lisp symbol
dnl 2 -- (optional) space-separated list of features to ‘require’
ELISP_CHECK([$1],[if ‘$1’ is defined],[dnl
m4_foreach([FEATURE],m4_split(m4_normalize($2)),[dnl
(require 'FEATURE)
])],[(fboundp '$1)])])dnl


AC_DEFUN([ELISP_CHECK_BOUNDP],[dnl
dnl 1 -- Emacs Lisp symbol
dnl 2 -- (optional) space-separated list of features to ‘require’
ELISP_CHECK([$1],[if ‘$1’ is defined],[dnl
m4_foreach([FEATURE],m4_split(m4_normalize($2)),[dnl
(require 'FEATURE)
])],[(boundp '$1)])])dnl


dnl History
dnl -  1 -- initial release
dnl -  2 -- send ‘$EMACS --batch’ out and err to config.log
dnl -  3 -- new macro: ELISP_CHECK_FEATURE
dnl -  4 -- use cache var prefix "elisp"

dnl Local variables:
dnl mode: autoconf
dnl End:
dnl elisp.m4 ends here
