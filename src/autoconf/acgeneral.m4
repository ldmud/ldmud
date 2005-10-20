dnl Parameterized macros that do not check for something specific.
dnl This file is part of Autoconf.
dnl Copyright (C) 1992, 1993 Free Software Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
dnl
dnl Written by David MacKenzie, with help from
dnl Franc,ois Pinard, Karl Berry, Richard Pixley, Ian Lance Taylor,
dnl Roland McGrath, and Noah Friedman.
dnl
changequote([,])dnl
undefine([eval])dnl
undefine([include])dnl
undefine([shift])dnl
undefine([format])dnl
dnl
ifdef([__gnu__], , [errprint(Autoconf requires GNU m4
)exit(1)])dnl
dnl
dnl
dnl Utility functions for stamping the configure script.
dnl
dnl
define(AC_ACVERSION, 1.7)dnl
dnl This is defined by the --version option of the autoconf script.
ifdef([AC_PRINT_VERSION], [errprint(Autoconf version AC_ACVERSION
)])dnl
dnl
dnl These are currently not used, for the sake of people who diff
dnl configure scripts and don't want spurious differences.
dnl But they are too clever to just delete.
dnl
define(AC_USER, [esyscmd(
changequote({,})dnl
# Extract the user name from the first pair of parentheses.
({sedcmd='s/[^(]*(\([^)]*\)).*/\1/';}
changequote([,])dnl
whoami || id|sed "$sedcmd") 2>${DEV_NULL}|tr -d '\012')])dnl
dnl
define(AC_HOST, [esyscmd((hostname || uname -n) 2>${DEV_NULL}|tr -d '\012')])dnl
dnl
define(AC_DATE, [esyscmd(date|tr -d '\012')])dnl
dnl
dnl
dnl Controlling Autoconf operation
dnl
dnl
dnl This is separate from AC_INIT to prevent GNU m4 1.0 from coredumping
dnl when AC_CONFIG_HEADER is used.
define(AC_NOTICE,
[# Guess values for system-dependent variables and create Makefiles.
# Generated automatically using autoconf.
dnl [#] Generated automatically using autoconf version AC_ACVERSION
dnl [#] by AC_USER@AC_HOST on AC_DATE
# Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Usage: configure [--srcdir=DIR] [--host=HOST] [--gas] [--nfp]
#        [--prefix=PREFIX] [--exec-prefix=PREFIX] [--with-PACKAGE[=VALUE]]
# Ignores all args except --srcdir, --prefix, --exec-prefix, and
# --with-PACKAGE[=VALUE] unless this script has special code to handle it.
])dnl
dnl
define(AC_PARSEARGS,
[for arg
do
  # Handle --exec-prefix with a space before the argument.
  if test x$next_exec_prefix = xyes; then exec_prefix=$arg; next_exec_prefix=
  # Handle --host with a space before the argument.
  elif test x$next_host = xyes; then next_host=
  # Handle --prefix with a space before the argument.
  elif test x$next_prefix = xyes; then prefix=$arg; next_prefix=
  # Handle --srcdir with a space before the argument.
  elif test x$next_srcdir = xyes; then srcdir=$arg; next_srcdir=
  else
    case $arg in
     # For backward compatibility, recognize -exec-prefix and --exec_prefix.
     -exec-prefix=* | --exec_prefix=* | --exec-prefix=* | --exec-prefi=* | --exec-pref=* | --exec-pre=* | --exec-pr=* | --exec-p=* | --exec-=* | --exec=* | --exe=* | --ex=* | --e=*)
changequote(,)dnl
	exec_prefix=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -exec-prefix | --exec_prefix | --exec-prefix | --exec-prefi | --exec-pref | --exec-pre | --exec-pr | --exec-p | --exec- | --exec | --exe | --ex | --e)
	next_exec_prefix=yes ;;

     -gas | --gas | --ga | --g) ;;

     -host=* | --host=* | --hos=* | --ho=* | --h=*) ;;
     -host | --host | --hos | --ho | --h)
	next_host=yes ;;

     -nfp | --nfp | --nf) ;;

     -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
changequote(,)dnl
	prefix=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
	next_prefix=yes ;;

     -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=* | --s=*)
changequote(,)dnl
	srcdir=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -srcdir | --srcdir | --srcdi | --srcd | --src | --sr | --s)
	next_srcdir=yes ;;

     -with-* | --with-*)
       package=`echo $arg|sed -e 's/-*with-//' -e 's/=.*//'`
       # Reject names that aren't valid shell variable names.
changequote(,)dnl
       if test -n "`echo $package| sed 's/[-a-zA-Z0-9_]//g'`"; then
changequote([,])dnl
         echo "configure: $package: invalid package name" >&2; exit 1
       fi
       package=`echo $package| sed 's/-/_/g'`
       case "$arg" in
changequote(,)dnl
         *=*) val="`echo $arg|sed 's/[^=]*=//'`" ;;
changequote([,])dnl
         *) val=1 ;;
       esac
       eval "with_$package='$val'" ;;

     -v | -verbose | --verbose | --verbos | --verbo | --verb | --ver | --ve | --v)
       verbose=yes ;;

     *) ;;
    esac
  fi
done
])dnl
dnl
define(AC_INIT,
[#!/bin/sh
AC_NOTICE
AC_PARSEARGS
AC_PREPARE($1)])dnl
dnl
define(AC_PREPARE,
[trap 'rm -fr conftest* confdefs* core; exit 1' 1 3 15
trap 'rm -f confdefs*' 0

# NLS nuisances.
# These must not be set unconditionally because not all systems understand
# e.g. LANG=C (notably SCO).
if test "${LC_ALL+set}" = 'set' ; then LC_ALL=C; export LC_ALL; fi
if test "${LANG+set}"   = 'set' ; then LANG=C;   export LANG;   fi

rm -f conftest* confdefs.h
# AIX cpp loses on an empty file, so make sure it contains at least a newline.
echo > confdefs.h
compile='${CC-cc} $CFLAGS conftest.c -o conftest $LIBS >${DEV_NULL} 2>&1'

# A filename unique to this package, relative to the directory that
# configure is in, which we can look for to find out if srcdir is correct.
unique_file=$1

# Find the source files, if location was not specified.
if test -z "$srcdir"; then
  srcdirdefaulted=yes
  # Try the directory containing this script, then `..'.
  prog=[$]0
changequote(,)dnl
  confdir=`echo $prog|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
  test "X$confdir" = "X$prog" && confdir=.
  srcdir=$confdir
  if test ! -r $srcdir/$unique_file; then
    srcdir=..
  fi
fi
if test ! -r $srcdir/$unique_file; then
  if test x$srcdirdefaulted = xyes; then
    echo "configure: Can not find sources in \`${confdir}' or \`..'." 1>&2
  else
    echo "configure: Can not find sources in \`${srcdir}'." 1>&2
  fi
  exit 1
fi
# Preserve a srcdir of `.' to avoid automounter screwups with pwd.
# But we can't avoid them for `..', to make subdirectories work.
case $srcdir in
  .|/*|~*) ;;
  *) srcdir=`cd $srcdir; pwd` ;; # Make relative path absolute.
esac

dnl This is effectively disabled because it is a nuisance if not using
dnl a configuration header file.
ifelse([AC_CONFIG_NAMES], [], [
# Check for an already configured srcdir.
if test -r $srcdir/config.status && \
  test z"`sh -c pwd`" != z"`(cd $srcdir; sh -c pwd)`"; then
  ${CAT} <<EOF >&2
configure: The directory tree \`${srcdir}' is being used
   as a build directory right now; it has been configured in its own
   right.  You cannot currently configure and build in both the source
   directory and another build directory.  To configure in this directory,
   you must first do \`make distclean' in ${srcdir},
   and then run configure again.
EOF
  exit 1
ifelse([not ready yet], [], [ XXX this should do the following:
* Edit "VPATH=..." into GNU vpath directives for %.c and %.h (%.y?).
* Add -DCONFIG_BROKETS to DEFS (config.h is not good enough, because it is
  used to FIND config.h).
${CAT} <<EOF >&2
configure: WARNING: The directory tree \`${srcdir}' is being used
   as a build directory right now; it has been configured in its own
   right.  To configure in another directory as well, you MUST
   use GNU make.  If you do not have GNU make, then you must
   now do \`make distclean' in ${srcdir},
   and then run ${progname} again.
EOF])dnl
fi])dnl

# Save the original args to write them into config.status later.
configure_args="[$]*"

if test -w /dev/null; then
  DEV_NULL=/dev/null
else
  DEV_NULL=dev.null
fi

#cat reading from stdin seems to be broken under MiNT on the Atari.
#It yields: 'cat: -: invalid memory block'
#working substitutes: less and dd (but dd is more verboose)
CAT=cat
echo x | cat > ${DEV_NULL} 2>&1 || CAT=less

if test ! -f ${CONFIG_SHELL-/bin/sh}; then
  CONFIG_SHELL=${SHELL-bash}
fi
])dnl
dnl
dnl Protects the argument from being diverted twice
dnl if this macro is called twice for it.
dnl Diversion 0 is the normal output.
dnl Diversion 1 is sed substitutions for output files.
dnl Diversion 2 is variable assignments for config.status.
define(AC_SUBST,
[ifdef([AC_SUBST_$1], ,
[define([AC_SUBST_$1], )dnl
divert(1)dnl
s%@$1@%[$]$1%g
divert(2)dnl
$1='[$]$1'
divert(0)dnl
])])dnl
dnl
define(AC_WITH,
[[#] check whether --with-$1 was given
withval="[$with_]patsubst($1,-,_)"
if test -n "$withval"; then
  ifelse([$2], , :, [$2])
ifelse([$3], , , [else
  $3
])dnl
fi
])dnl
dnl
dnl Guess the value for the `prefix' variable by looking for
dnl the argument program along PATH and taking its parent.
dnl Example: if the argument is `gcc' and we find /usr/local/gnu/bin/gcc,
dnl set `prefix' to /usr/local/gnu.
define(AC_PREFIX,
[if test -z "$prefix"
then
  echo checking for $1 to derive installation directory prefix
  IFS="${IFS= 	}"; saveifs="$IFS"; IFS="$IFS:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    if test $dir != . && test -f $dir/$1; then
changequote(,)dnl
      # Not all systems have dirname.
      prefix=`echo $dir|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
      break
    fi
  done
  IFS="$saveifs"
  echo "	chose installation directory prefix ${prefix}"
fi
])dnl
dnl
define(AC_CONFIG_HEADER, [define(AC_CONFIG_NAMES, $1)])dnl
dnl
define(AC_DOREV, [#!/bin/sh
# From configure.in $1
])dnl
define(AC_REVISION, [AC_DOREV(translit($1,$"))])dnl
dnl
dnl
dnl Several simple subroutines to do various flavors of quoting.
dnl
dnl Quote $1 against shell "s.
define(AC_QUOTE_DQUOTE, [dnl We use \1 instead of \& to avoid an m4 1.0.3 bug.
patsubst($1, changequote(,)\([$"`\\]\)changequote([,]), \\\1)])dnl
dnl
dnl Quote $1 against shell 's.
define(AC_QUOTE_SQUOTE, [patsubst($1, ', '\\'')])dnl
dnl
dnl Quote $1 against shell here documents (<<EOF).
define(AC_QUOTE_HERE, [changequote({,})dnl
dnl We use \1 instead of \& to avoid an m4 1.0.3 bug.
patsubst(patsubst($1, \(\\[$`\\]\), \\\1), \([$`]\), \\\1){}dnl
changequote([,])])dnl
dnl
dnl Quote $1 against the right hand side of a sed substitution.
define(AC_QUOTE_SED, [changequote({,})dnl
dnl We use \1 instead of \& to avoid an m4 1.0.3 bug.
dnl % and @ are commonly used as the sed s separator character.
patsubst($1, \([&\\%@]\), \\\1){}dnl
changequote([,])])dnl
dnl
dnl Quote $1 against tokenization.
define(AC_QUOTE_TOKEN, [changequote({,})dnl
patsubst($1, \([	 ]\), \\\1){}dnl
changequote([,])])dnl
dnl
dnl Subroutines of AC_DEFINE.  Does more quoting magic than any sane person
dnl should be able to understand.  The point of it all is that what goes into
dnl Makefile et al should be verbatim what was written in configure.in.
define(AC_DEFINE_QUOTE, [dnl
AC_QUOTE_TOKEN(AC_QUOTE_SQUOTE(AC_QUOTE_DQUOTE($1)))])dnl
dnl
define(AC_DEFINE_SEDQUOTE, [dnl
AC_QUOTE_DQUOTE(AC_QUOTE_HERE(AC_QUOTE_HERE(AC_QUOTE_SED($1))))])dnl
dnl
dnl Don't compare $2 to a blank, so we can support "-Dfoo=".
dnl If creating a configuration header file, we add
dnl commands to SEDDEFS to define the variable.  SED[due][ABCD]
dnl get defined in config.status.  Here we just insert the
dnl variable parts of the string: the variable name to define
dnl and the value to give it.
dnl The newlines around the curly braces prevent sh syntax errors.
define(AC_DEFINE,[
{
dnl Uniformly use AC_DEFINE_[SED]QUOTE, so callers of AC_DEFINE_UNQUOTED
dnl can use AC_QUOTE_* manually if they want to.
test -n "$verbose" && \
ifelse($#, 2,
[define([AC_VAL], $2)dnl
echo "	defining" $1 to be ifelse(AC_VAL,, empty, AC_DEFINE_QUOTE(AC_VAL))],
[define([AC_VAL], 1)dnl
echo "	defining $1"])
dnl
echo "[#][define]" $1 AC_DEFINE_QUOTE(AC_VAL) >> confdefs.h
dnl Define DEFS even if AC_CONFIG_NAMES for use in user case statements.
DEFS="$DEFS -D$1=AC_DEFINE_QUOTE(AC_VAL)"
ifdef([AC_CONFIG_NAMES],
SEDDEFS="dnl
${SEDDEFS}\${SEDdA}$1\${SEDdB}$1\${SEDdC}AC_DEFINE_SEDQUOTE(AC_VAL)\${SEDdD}
\${SEDuA}$1\${SEDuB}$1\${SEDuC}AC_DEFINE_SEDQUOTE(AC_VAL)\${SEDuD}
\${SEDeA}$1\${SEDeB}$1\${SEDeC}AC_DEFINE_SEDQUOTE(AC_VAL)\${SEDeD}
"
)dnl
}
])dnl
dnl
dnl Unsafe version of AC_DEFINE.
dnl Users are responsible for the quoting nightmare.
dnl Well, not all of it.  We need to pull the identify function out to
dnl the top level, because m4 doesn't really support nested functions;
dnl it doesn't distinguish between the arguments to the outer
dnl function, which should be expanded, and the arguments to the inner
dnl function, which shouldn't yet.
define(AC_IDENTITY,$1)dnl
define(AC_DEFINE_UNQUOTED,[dnl
pushdef([AC_DEFINE_QUOTE],defn([AC_IDENTITY]))dnl
pushdef([AC_DEFINE_SEDQUOTE],defn([AC_IDENTITY]))dnl
AC_DEFINE($1,$2)dnl
popdef([AC_DEFINE_SEDQUOTE])dnl
popdef([AC_DEFINE_QUOTE])dnl
])dnl
dnl
define(AC_BEFORE,
[ifdef([AC_PROVIDE_$2], [errprint(__file__:__line__: [$2 was called before $1
])])])dnl
dnl
define(AC_REQUIRE,
[ifdef([AC_PROVIDE_$1],,[indir([$1])
])])dnl
dnl
define(AC_PROVIDE,
[define([AC_PROVIDE_$1],)])dnl
dnl
define(AC_OBSOLETE,
[errprint(__file__:__line__: warning: [$1] is obsolete[$2]
)])dnl
dnl
dnl
dnl Checks for kinds of features
dnl
dnl
define(AC_PROGRAM_CHECK,
[if test -z "[$]$1"; then
  # Extract the first word of `$2', so it can be a program name with args.
  set dummy $2; word=[$]2
  echo checking for $word
  IFS="${IFS= 	}"; saveifs="$IFS"; IFS="${IFS}:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    if test -f $dir/$word; then
      $1="$3"
      break
    fi
  done
  IFS="$saveifs"
fi
ifelse([$4],,, [test -z "[$]$1" && $1="$4"])
test -n "[$]$1" && test -n "$verbose" && echo "	setting $1 to [$]$1"
AC_SUBST($1)dnl
])dnl
dnl
define(AC_PROGRAMS_CHECK,
[for p in $2
do
AC_PROGRAM_CHECK($1, [$]p, [$]p, )
test -n "[$]$1" && break
done
ifelse([$3],,, [test -n "[$]$1" || $1="$3"
])])dnl
dnl
define(AC_HEADER_EGREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])echo '#include "confdefs.h"
#include <$2>' > conftest.c
eval "$CPP conftest.c > conftest.out 2>&1"
if egrep "$1" conftest.out >${DEV_NULL} 2>&1; then
  ifelse([$3], , :, [rm -rf conftest*
  $3
])
ifelse([$4], , , [else
  rm -rf conftest*
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
dnl Because this macro is used by AC_GCC_TRADITIONAL, which must come early,
dnl it is not included in AC_BEFORE checks.
define(AC_PROGRAM_EGREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])${CAT} > conftest.c <<EOF
#include "confdefs.h"
[$2]
EOF
eval "$CPP conftest.c > conftest.out 2>&1"
if egrep "$1" conftest.out >${DEV_NULL} 2>&1; then
  ifelse([$3], , :, [rm -rf conftest*
  $3
])
ifelse([$4], , , [else
  rm -rf conftest*
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
define(AC_HEADER_CHECK,
[echo checking for $1
ifelse([$3], , [AC_TEST_CPP([#include <$1>], [$2])],
[AC_TEST_CPP([#include <$1>], [$2], [$3])])
])dnl
dnl
define(AC_COMPILE_CHECK,
[AC_PROVIDE([$0])dnl
ifelse([$1], , , [echo checking for $1]
)dnl
${CAT} > conftest.c <<EOF
#include "confdefs.h"
[$2]
int main() { exit(0); }
int t() { [$3] }
EOF
dnl Don't try to run the program, which would prevent cross-configuring.
if eval $compile; then
  ifelse([$4], , :, [rm -rf conftest*
  $4
])
ifelse([$5], , , [else
  rm -rf conftest*
  $5
])dnl
fi
rm -f conftest*]
)dnl
dnl
define(AC_TEST_PROGRAM,
[AC_PROVIDE([$0])ifelse([$4], , , [AC_REQUIRE([AC_CROSS_CHECK])if test -n "$cross_compiling"
then
  $4
else
])dnl
${CAT} > conftest.c <<EOF
#include "confdefs.h"
[$1]
EOF
eval $compile
#beware: bash 1.05.1 sends exit signals to the main shell from subshells
if test -s conftest && (trap '' 0; ./conftest; exit) 2>${DEV_NULL}; then
  ifelse([$2], , :, [$2
])
ifelse([$3], , , [else
  $3
])dnl
fi
ifelse([$4], , , fi
)dnl
rm -fr conftest*])dnl
dnl
define(AC_TEST_CPP,
[AC_REQUIRE([AC_PROG_CPP])${CAT} > conftest.c <<EOF
#include "confdefs.h"
[$1]
EOF
dnl Some shells (Coherent) do redirections in the wrong order, so need
dnl the parens.
err=`eval "($CPP conftest.c >${DEV_NULL}) 2>&1"`
if test -z "$err"; then
  ifelse([$2], , :, [rm -rf conftest*
  $2
])
ifelse([$3], , , [else
  rm -rf conftest*
  $3
])dnl
fi
rm -f conftest*])dnl
dnl
define(AC_REPLACE_FUNCS,
[for func in $1
do
AC_COMPILE_CHECK([${func}], [#include <ctype.h>], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_${func}) || defined (__stub___${func})
choke me
#else
/* Override any gcc2 internal prototype to avoid an error.  */
extern char ${func}(); ${func}();
#endif
], , [LIBOBJS="$LIBOBJS ${func}.o"
test -n "$verbose" && echo "	using ${func}.o instead"])
done
AC_SUBST(LIBOBJS)dnl
])dnl
dnl
define(AC_FUNC_CHECK,
[ifelse([$3], , [AC_COMPILE_CHECK($1, [#include <ctype.h>], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_$1) || defined (__stub___$1)
choke me
#else
/* Override any gcc2 internal prototype to avoid an error.  */
extern char $1(); $1();
#endif
],
$2)], [AC_COMPILE_CHECK($1, [#include <ctype.h>], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_$1) || defined (__stub___$1)
choke me
#else
/* Override any gcc2 internal prototype to avoid an error.  */
extern char $1(); $1();
#endif
],
$2, $3)])dnl
])dnl
dnl
define(AC_CAPFUNC,
[#At least my Atari has no tr .
capfunc='echo HAVE_$func|sed y~abcdefghijklmnopqrstuvwxyz./~ABCDEFGHIJKLMNOPQRSTUVWXYZ__~'])dnl
dnl
define(AC_HAVE_FUNCS,
[AC_REQUIRE([AC_CAPFUNC])dnl
for func in $1
do
trfunc=`eval ${capfunc}`
AC_FUNC_CHECK(${func},
AC_DEFINE(${trfunc}))dnl
done
])dnl
dnl
define(AC_HAVE_HEADERS,
[AC_REQUIRE([AC_CAPFUNC])dnl
for func in $1
do
trhdr=`eval ${capfunc}`
AC_HEADER_CHECK(${func},
AC_DEFINE(${trhdr}))dnl
done
])dnl
dnl
define(AC_HAVE_LIBRARY, [dnl
changequote(/,/)dnl
define(/libname/, dnl
patsubst(patsubst($1, /lib\([^\.]*\)\.a/, /\1/), /-l/, //))dnl
changequote([,])dnl
LIBS_save="${LIBS}"
LIBS="${LIBS} -l[]libname[]"
have_lib=""
AC_COMPILE_CHECK([-l[]libname[]], , [main();], [have_lib="1"])dnl
LIBS="${LIBS_save}"
ifelse($#, 1, [dnl
if test -n "${have_lib}"; then
   AC_DEFINE([HAVE_LIB]translit(libname, [a-z], [A-Z]))
   LIBS="${LIBS} -l[]libname[]"
fi
undefine(libname)dnl
], [dnl
if test -n "${have_lib}"; then
   :; $2
else
   :; $3
fi
])])dnl
dnl
dnl
dnl The big finish
dnl
dnl
define(AC_OUTPUT,
[changequote(,)dnl
# Set default prefixes.
if test -n "$prefix"; then
  test -z "$exec_prefix" && exec_prefix='${prefix}'
  prsub="s%^prefix\\([ 	]*\\)=\\([ 	]*\\).*$%prefix\\1=\\2$prefix%"
fi
if test -n "$exec_prefix"; then
  prsub="$prsub
s%^exec_prefix\\([ 	]*\\)=\\([ 	]*\\).*$%exec_prefix\\1=\\2$exec_prefix%"
fi
# Quote sed substitution magic chars in DEFS.
${CAT} >conftest.def <<EOF
$DEFS
EOF
escape_ampersand_and_backslash='s%[&\\]%\\&%g'
DEFS=`sed "$escape_ampersand_and_backslash" <conftest.def`
rm -f conftest.def
# Substitute for predefined variables.
changequote([,])dnl
AC_SUBST(LIBS)dnl
AC_SUBST(srcdir)dnl
dnl Substituting for DEFS would confuse sed if it contains multiple lines.
ifdef([AC_CONFIG_NAMES],
[divert(1)dnl
s%@DEFS@%-DHAVE_CONFIG_H%],
[divert(1)dnl
s%@DEFS@%$DEFS%]
[divert(2)dnl
DEFS='$DEFS'
])dnl
divert(2)dnl
prefix='$prefix'
exec_prefix='$exec_prefix'
prsub='$prsub'
extrasub='$extrasub'
divert(0)dnl

trap 'rm -f config.status; exit 1' 1 3 15
echo creating config.status
rm -f config.status
${CAT} > config.status <<EOF
#!/bin/sh
# Generated automatically by configure.
# Run this file to recreate the current configuration.
# This directory was configured as follows,
dnl hostname on some systems (SVR3.2, Linux) returns a bogus exit status,
dnl so uname gets run too.
# on host `(hostname || uname -n) 2>${DEV_NULL} | sed 1q`:
#
[#] [$]0 [$]configure_args

CAT='${CAT}'
CONFIG_SHELL='${CONFIG_SHELL}'
DEV_NULL='${DEV_NULL}'

for arg
do
  case "[\$]arg" in
    -recheck | --recheck | --rechec | --reche | --rech | --rec | --re | --r)
    echo running [\$]{CONFIG_SHELL-/bin/sh} [$]0 [$]configure_args
    exec [\$]{CONFIG_SHELL-/bin/sh} [$]0 [$]configure_args ;;
    *) echo "Usage: config.status [--recheck]" 2>&1; exit 1 ;;
  esac
done

ifdef([AC_CONFIG_NAMES],
[trap 'rm -fr $1 AC_CONFIG_NAMES conftest*; exit 1' 1 3 15],
[trap 'rm -f $1; exit 1' 1 3 15])
dnl Insert the variable assignments.
undivert(2)dnl
EOF
${CAT} >> config.status <<\EOF

top_srcdir=$srcdir

CONFIG_FILES=${CONFIG_FILES-"$1"}
for file in .. ${CONFIG_FILES}; do if test "x$file" != x..; then
  srcdir=$top_srcdir
  # Remove last slash and all that follows it.  Not all systems have dirname.
changequote(,)dnl
  dir=`echo $file|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
  if test "$dir" != "$file"; then
    test "$top_srcdir" != . && srcdir=$top_srcdir/$dir
    test ! -d $dir && mkdir $dir
  fi
  echo creating $file
  rm -f $file
  echo "# Generated automatically from `echo $file|sed 's|.*/||'`.in by configure." > $file
  sed -e "
$prsub
dnl Shell code in configure.in might set extrasub.
$extrasub
dnl Insert the sed substitutions.
undivert(1)dnl
" $top_srcdir/${file}.in >> $file
fi; done
AC_OUTPUT_HEADER
$2
exit 0
EOF
chmod +x config.status
${CONFIG_SHELL-/bin/sh} config.status
])dnl
dnl This is a subroutine of AC_OUTPUT, broken out primarily to avoid bugs
dnl with long definitions in GNU m4 1.0.  This is called inside a quoted
dnl here document whose contents are going into config.status.
define(AC_OUTPUT_HEADER,[dnl
ifdef([AC_CONFIG_NAMES],[dnl
changequote(<<,>>)dnl

CONFIG_HEADERS=${CONFIG_HEADERS-"AC_CONFIG_NAMES"}
for file in .. ${CONFIG_HEADERS}; do if test "x$file" != x..; then
echo creating $file

# These sed commands are put into SEDDEFS when defining a macro.
# They are broken into pieces to make the sed script easier to manage.
# They are passed to sed as "A NAME B NAME C VALUE D", where NAME
# is the cpp macro being defined and VALUE is the value it is being given.
# Each defining turns into a single global substitution command.
#
# SEDd sets the value in "#define NAME VALUE" lines.
SEDdA='s@^\([ 	]*\)#\([ 	]*define[ 	][ 	]*\)'
SEDdB='\([ 	][ 	]*\)[^ 	]*@\1#\2'
SEDdC='\3'
SEDdD='@g'
# SEDu turns "#undef NAME" with trailing blanks into "#define NAME VALUE".
SEDuA='s@^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
SEDuB='\([ 	]\)@\1#\2define\3'
SEDuC=' '
SEDuD='\4@g'
# SEDe turns "#undef NAME" without trailing blanks into "#define NAME VALUE".
SEDeA='s@^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
SEDeB='<<$>>@\1#\2define\3'
SEDeC=' '
SEDeD='@g'
changequote([,])dnl
rm -f conftest.sed
EOF
# Turn off quoting long enough to insert the sed commands.
rm -f conftest.sh
${CAT} > conftest.sh <<EOF
$SEDDEFS
EOF

# Break up $SEDDEFS (now in conftest.sh) because some shells have a limit
# on the size of here documents.

# Maximum number of lines to put in a single here document.
maxshlines=9

while :
do
  # wc gives bogus results for an empty file on some systems.
  lines=`grep -c . conftest.sh`
  if test -z "$lines" || test "$lines" -eq 0; then break; fi
  rm -f conftest.s1 conftest.s2
  sed ${maxshlines}q conftest.sh > conftest.s1 # Like head -20.
  sed 1,${maxshlines}d conftest.sh > conftest.s2 # Like tail +21.
  # Write a limited-size here document to append to conftest.sed.
  echo '${CAT} >> conftest.sed <<CONFEOF' >> config.status
  ${CAT} conftest.s1 >> config.status
  echo 'CONFEOF' >> config.status
  rm -f conftest.s1 conftest.sh
  mv conftest.s2 conftest.sh
done
rm -f conftest.sh

# Now back to your regularly scheduled config.status.
${CAT} >> config.status <<\EOF
# This sed command replaces #undef's with comments.  This is necessary, for
# example, in the case of _POSIX_SOURCE, which is predefined and required
# on some systems where configure will not decide to define it in
[#] AC_CONFIG_NAMES.
${CAT} >> conftest.sed <<\CONFEOF
changequote(,)dnl
s,^[ 	]*#[ 	]*undef[ 	][ 	]*[a-zA-Z_][a-zA-Z_0-9]*,/* & */,
changequote([,])dnl
CONFEOF
rm -f conftest.h
# Break up the sed commands because old seds have small limits.
maxsedlines=20
cp $top_srcdir/$file.in conftest.h1
while :
do
  lines=`grep -c . conftest.sed`
  if test -z "$lines" || test "$lines" -eq 0; then break; fi
  rm -f conftest.s1 conftest.s2 conftest.h2
  sed ${maxsedlines}q conftest.sed > conftest.s1 # Like head -20.
  sed 1,${maxsedlines}d conftest.sed > conftest.s2 # Like tail +21.
  sed -f conftest.s1 < conftest.h1 > conftest.h2
  rm -f conftest.s1 conftest.h1 conftest.sed
  mv conftest.h2 conftest.h1
  mv conftest.s2 conftest.sed
done
rm -f conftest.sed conftest.h
echo "/* $file.  Generated automatically by configure.  */" > conftest.h
${CAT} conftest.h1 >> conftest.h
rm -f conftest.h1
if cmp -s $file conftest.h 2>${DEV_NULL}; then
  # The file exists and we would not be changing it.
  echo "$file is unchanged"
  rm -f conftest.h
else
  rm -f $file
  mv conftest.h $file
fi
fi; done

])])dnl
