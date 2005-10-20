dnl Macros that test for specific features.
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
dnl
dnl checks for programs
dnl
dnl
define(AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])AC_PROVIDE([$0])AC_SUBST(CC)dnl
# gcc might be an alias to the actual pathname when using bash.
if test -z "$CC"; then
  gcc -v > ${DEV_NULL} 2>&1 && CC="gcc"
  test -z "$CC" && CC="cc"
  test -n "$CC" && test -n "$verbose" && echo "   setting CC to $CC"
fi
# Find out if we are using GNU C, under whatever name.
${CAT} > conftest.c <<EOF
#ifdef __GNUC__
  yes
#endif
EOF
${CC-cc} -E conftest.c > conftest.out 2>&1
if egrep yes conftest.out >${DEV_NULL} 2>&1; then
  GCC=1 # For later tests.
fi
rm -f conftest*
])dnl
dnl
define(AC_GCC_TRADITIONAL,
[AC_REQUIRE([AC_PROG_CC])AC_REQUIRE([AC_PROG_CPP])if test -n "$GCC"; then
  echo checking whether -traditional is needed
changequote(,)dnl
  pattern="Autoconf.*'x'"
changequote([,])dnl
  prog='#include <sgtty.h>
Autoconf TIOCGETP'
  AC_PROGRAM_EGREP($pattern, $prog, need_trad=1)

  if test -z "$need_trad"; then
    prog='#include <termio.h>
Autoconf TCGETA'
    AC_PROGRAM_EGREP($pattern, $prog, need_trad=1)
  fi
  test -n "$need_trad" && CC="$CC -traditional"
fi
])dnl
dnl
define(AC_MINUS_C_MINUS_O,
[echo checking whether $CC and cc understand -c and -o together
echo 'foo(){}' > conftest.c
# Make sure it works both with $CC and with simple cc.
# We do the test twice because some compilers refuse to overwrite an
# existing .o file with -o, though they will create one.
if ${CC-cc} -c conftest.c -o conftest.o >${DEV_NULL} 2>&1 \
 && test -f conftest.o && ${CC-cc} -c conftest.c -o conftest.o >${DEV_NULL} 2>&1
then
  # Test first that cc exists at all.
  if cc -c conftest.c >${DEV_NULL} 2>&1
  then
    if cc -c conftest.c -o conftest2.o >${DEV_NULL} 2>&1 && \
       test -f conftest2.o && cc -c conftest.c -o conftest2.o >${DEV_NULL} 2>&1
    then
      :
    else
      AC_DEFINE(NO_MINUS_C_MINUS_O)
    fi
  fi
else
  AC_DEFINE(NO_MINUS_C_MINUS_O)
fi
rm -f conftest*
])dnl
dnl
define(AC_PROG_RANLIB, [AC_PROGRAM_CHECK(RANLIB, ranlib, ranlib, :)])dnl
dnl
define(AC_PROG_AWK, [AC_PROGRAMS_CHECK(AWK, mawk gawk nawk awk,)])dnl
dnl
define(AC_PROG_YACC,[AC_PROGRAMS_CHECK(YACC, 'bison -y' byacc, yacc)])dnl
dnl
define(AC_PROG_CPP,
[AC_PROVIDE([$0])echo checking how to run the C preprocessor
if test -z "$CPP"; then
  # This must be in double quotes, not single quotes, because CPP may get
  # substituted into the Makefile and ``${CC-cc}'' will simply confuse
  # make.  It must be expanded now.
  CPP="${CC-cc} -E"
dnl On the NeXT, cc -E appears to run the code through the compiler's parser,
dnl not just through cpp.
  AC_TEST_CPP([#include <stdio.h>
Syntax Error], , CPP=/lib/cpp)
fi
test ".${verbose}" != "." && echo "	setting CPP to $CPP"
AC_SUBST(CPP)dnl
])dnl
dnl
define(AC_PROG_LEX,
[AC_PROVIDE([$0])AC_PROGRAM_CHECK(LEX, flex, flex, lex)
if test -z "$LEXLIB"
then
  case "$LEX" in
  flex*) AC_HAVE_LIBRARY(fl, LEXLIB="-lfl") ;;
  *) LEXLIB="-ll" ;;
  esac
fi
AC_SUBST(LEXLIB)])dnl
dnl
define(AC_DECLARE_YYTEXT,
[AC_REQUIRE([AC_PROG_CPP])AC_REQUIRE([AC_PROG_LEX])dnl
echo checking how to declare yytext
# Figure out what yytext is by creating a minimal parser and
# examining the (preprocessed, in case macros are used) output.
if test "z${DECLARE_YYTEXT}" = "z"; then
changequote(,)dnl
  echo '%%
%%' | ${LEX}
  if test -f lex.yy.c; then
    LEX_OUTPUT_ROOT=lex.yy
  elif test -f lexyy.c; then
    LEX_OUTPUT_ROOT=lexyy
  else
    # Don't know what to do here.
    echo "Can't find output from $LEX; assuming lex.yy.c." 1>&2
    LEX_OUTPUT_ROOT=lex.yy
  fi
  AC_SUBST(LEX_OUTPUT_ROOT)dnl
  DECLARE_YYTEXT=`eval ${CPP} "${LEX_OUTPUT_ROOT}.c" |
    sed -n '/extern.*yytext[^a-zA-Z0-9_]/s/^.*extern/extern/p'`
  rm -f "${LEX_OUTPUT_ROOT}.c"
changequote([,])dnl
fi
AC_DEFINE_UNQUOTED(DECLARE_YYTEXT, \"$DECLARE_YYTEXT\")dnl
])dnl
dnl
define(AC_PROG_INSTALL,
[# Make sure to not get the incompatible SysV /etc/install and
# /usr/sbin/install, which might be in PATH before a BSD-like install,
# or the SunOS /usr/etc/install directory, or the AIX /bin/install,
# or the AFS install, which mishandles nonexistent args, or
# /usr/ucb/install on SVR4, which tries to use the nonexistent group
# `staff'.  On most BSDish systems install is in /usr/bin, not /usr/ucb
# anyway.  Sigh.
if test "z${INSTALL}" = "z" ; then
  echo checking for install
  IFS="${IFS= 	}"; saveifs="$IFS"; IFS="${IFS}:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    case $dir in
    /etc|/usr/sbin|/usr/etc|/usr/afsws/bin|/usr/ucb) ;;
    *)
      if test -f $dir/installbsd; then
	INSTALL="$dir/installbsd -c" # OSF1
	INSTALL_PROGRAM='$(INSTALL)'
	INSTALL_DATA='$(INSTALL) -m 644'
	break
      fi
      if test -f $dir/install; then
	if grep dspmsg $dir/install >${DEV_NULL} 2>&1; then
	  : # AIX
	else
	  INSTALL="$dir/install -c"
	  INSTALL_PROGRAM='$(INSTALL)'
	  INSTALL_DATA='$(INSTALL) -m 644'
	  break
	fi
      fi
      ;;
    esac
  done
  IFS="$saveifs"
fi
INSTALL=${INSTALL-cp}
AC_SUBST(INSTALL)dnl
test -n "$verbose" && echo "	setting INSTALL to $INSTALL"
INSTALL_PROGRAM=${INSTALL_PROGRAM-'$(INSTALL)'}
AC_SUBST(INSTALL_PROGRAM)dnl
test -n "$verbose" && echo "	setting INSTALL_PROGRAM to $INSTALL_PROGRAM"
INSTALL_DATA=${INSTALL_DATA-'$(INSTALL)'}
AC_SUBST(INSTALL_DATA)dnl
test -n "$verbose" && echo "	setting INSTALL_DATA to $INSTALL_DATA"
])dnl
dnl
define(AC_LN_S,
[echo checking for ln -s
rm -f conftestdata
if ln -s X conftestdata 2>${DEV_NULL}
then
  rm -f conftestdata
  LN_S="ln -s"
else
  LN_S=ln
fi
AC_SUBST(LN_S)
])dnl
dnl
define(AC_RSH,
[echo checking for remote shell
if test -f /usr/ucb/rsh || test -f /usr/bin/remsh || test -f /usr/bin/rsh ||
  test -f /usr/bsd/rsh || test -f /usr/bin/nsh; then
  RTAPELIB=rtapelib.o
else
  AC_HEADER_CHECK(netdb.h, AC_DEFINE(HAVE_NETDB_H) RTAPELIB=rtapelib.o,
    AC_DEFINE(NO_REMOTE))
fi
AC_SUBST(RTAPELIB)dnl
])dnl
dnl
dnl
dnl checks for header files
dnl
dnl
define(AC_STDC_HEADERS,
[AC_REQUIRE([AC_PROG_CPP])dnl
echo checking for ANSI C header files
AC_TEST_CPP([#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>],
[# SunOS 4.x string.h does not declare mem*, contrary to ANSI.
AC_HEADER_EGREP(memchr, string.h,
# SGI's /bin/cc from Irix-4.0.5 gets non-ANSI ctype macros unless using -ansi.
AC_TEST_PROGRAM([#include <ctype.h>
#define ISLOWER(c) ('a' <= (c) && (c) <= 'z')
#define TOUPPER(c) (ISLOWER(c) ? 'A' + ((c) - 'a') : (c))
#define XOR(e,f) (((e) && !(f)) || (!(e) && (f)))
int main () { int i; for (i = 0; i < 256; i++)
if (XOR (islower (i), ISLOWER (i)) || toupper (i) != TOUPPER (i)) exit(2);
exit (0); }
],AC_DEFINE(STDC_HEADERS)))])
])dnl
dnl
define(AC_UNISTD_H, [AC_OBSOLETE([$0], [; instead use AC_HAVE_HEADERS(unistd.h)])AC_HEADER_CHECK(unistd.h,
  AC_DEFINE(HAVE_UNISTD_H))])dnl
dnl
define(AC_USG,
[AC_OBSOLETE([$0], [; instead use AC_HAVE_HEADERS(string.h) and HAVE_STRING_H])AC_COMPILE_CHECK([BSD string and memory functions],
[#include <strings.h>], [rindex(0, 0); bzero(0, 0);], , AC_DEFINE(USG))])dnl
dnl
dnl
dnl If memchr and the like aren't declared in <string.h>, include <memory.h>.
dnl To avoid problems, don't check for gcc2 built-ins.
define(AC_MEMORY_H,
[AC_OBSOLETE([$0], [; instead use AC_HAVE_HEADERS(memory.h) and HAVE_MEMORY_H])echo checking whether string.h declares mem functions
AC_HEADER_EGREP(memchr, string.h, ,
  AC_HEADER_CHECK(memory.h, AC_DEFINE(NEED_MEMORY_H)))]
)dnl
dnl
define(AC_MAJOR_HEADER,
[AC_COMPILE_CHECK([major, minor and makedev header],
[#include <sys/types.h>],
[return makedev(0, 0);], makedev=1)
if test -z "$makedev"; then
AC_HEADER_CHECK(sys/mkdev.h, AC_DEFINE(MAJOR_IN_MKDEV) makedev=1)
fi
if test -z "$makedev"; then
AC_HEADER_CHECK(sys/sysmacros.h, AC_DEFINE(MAJOR_IN_SYSMACROS))
fi]
)dnl
dnl
define(AC_DIR_HEADER,
[AC_PROVIDE([$0])echo checking for directory library header
dirheader=
AC_DIR_HEADER_CHECK(dirent.h, DIRENT)
AC_DIR_HEADER_CHECK(sys/ndir.h, SYSNDIR)
AC_DIR_HEADER_CHECK(sys/dir.h, SYSDIR)
AC_DIR_HEADER_CHECK(ndir.h, NDIR)

echo checking for closedir return value
AC_TEST_PROGRAM([#include <sys/types.h>
#include <$dirheader>
int closedir(); main() { exit(closedir(opendir(".")) != 0); }], ,
AC_DEFINE(VOID_CLOSEDIR))
])dnl
dnl Subroutine of AC_DIR_HEADER.
dnl ??? I tried to put this define inside AC_DIR_HEADER, but when I did
dnl that, $1 and $2 did not get expanded. --roland
dnl Check if $1 is the winning directory library header file.
dnl It must not only exist, but also correctly define the `DIR' type.
dnl If it is really winning, define $2 and set shell var `dirheader' to $1.
define(AC_DIR_HEADER_CHECK, [dnl
if test -z "$dirheader"; then
  AC_COMPILE_CHECK($1, [#include <sys/types.h>
#include <]$1[>],
	  	   [DIR *dirp = 0;],
		   AC_DEFINE($2) dirheader=$1)dnl
fi])dnl
dnl
dnl
dnl checks for typedefs
dnl
dnl
define(AC_GETGROUPS_T,
[AC_REQUIRE([AC_UID_T])dnl
echo checking for type of array argument to getgroups
changequote(,)dnl
dnl Do not put single quotes in the C program text!!
prog='/* Thanks to Mike Rendell for this test.  */
#include <sys/types.h>
#define NGID 256
#undef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
main()
{
  gid_t gidset[NGID];
  int i, n;
  union { gid_t gval; long lval; }  val;

  val.lval = -1;
  for (i = 0; i < NGID; i++)
    gidset[i] = val.gval;
  n = getgroups (sizeof (gidset) / MAX (sizeof (int), sizeof (gid_t)) - 1,
                 gidset);
  /* Exit non-zero if getgroups seems to require an array of ints.  This
     happens when gid_t is short but getgroups modifies an array of ints.  */
  exit ((n > 0 && gidset[n] != val.gval) ? 1 : 0);
}'
changequote([,])dnl
AC_TEST_PROGRAM([$prog],
		AC_DEFINE(GETGROUPS_T, gid_t),
		AC_DEFINE(GETGROUPS_T, int))
])dnl
dnl
define(AC_UID_T,
[AC_PROVIDE([$0])echo checking for uid_t in sys/types.h
AC_HEADER_EGREP(uid_t, sys/types.h, ,
  AC_DEFINE(uid_t, int) AC_DEFINE(gid_t, int))])dnl
dnl
define(AC_SIZE_T,
[echo checking for size_t in sys/types.h
AC_HEADER_EGREP(size_t, sys/types.h, , AC_DEFINE(size_t, unsigned))])dnl
dnl
define(AC_PID_T,
[AC_PROVIDE([$0])echo checking for pid_t in sys/types.h
AC_HEADER_EGREP(pid_t, sys/types.h, , AC_DEFINE(pid_t, int))])dnl
dnl
define(AC_OFF_T,
[AC_PROVIDE([$0])echo checking for off_t in sys/types.h
AC_HEADER_EGREP(off_t, sys/types.h, , AC_DEFINE(off_t, long))])dnl
dnl
define(AC_MODE_T,
[echo checking for mode_t in sys/types.h
AC_HEADER_EGREP(mode_t, sys/types.h, , AC_DEFINE(mode_t, int))])dnl
dnl
define(AC_RETSIGTYPE,
[AC_PROVIDE([$0])AC_COMPILE_CHECK([return type of signal handlers],
[#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern void (*signal ()) ();],
[int i;],
[AC_DEFINE(RETSIGTYPE, void)],
[AC_DEFINE(RETSIGTYPE, int)])]
)dnl
dnl
dnl
dnl checks for functions
dnl
dnl
define(AC_VPRINTF,
[AC_COMPILE_CHECK([vprintf], , [vprintf();], AC_DEFINE(HAVE_VPRINTF),
vprintf_missing=1)
if test -n "$vprintf_missing"; then
AC_COMPILE_CHECK([_doprnt], , [_doprnt();], AC_DEFINE(HAVE_DOPRNT))
fi
])dnl
dnl
define(AC_VFORK,
[AC_REQUIRE([AC_PID_T])AC_HEADER_CHECK(vfork.h, AC_DEFINE(HAVE_VFORK_H))
echo checking for working vfork
AC_REQUIRE([AC_RETSIGTYPE])
AC_TEST_PROGRAM([/* Thanks to Paul Eggert for this test.  */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
static int signalled;
static RETSIGTYPE catch (s) int s; { signalled = 1; }
main() {
  pid_t parent = getpid ();
  pid_t child;

  signal (SIGINT, catch);

  child = vfork ();

  if (child == 0) {
    /* On sparc systems, changes by the child to local and incoming
       argument registers are propagated back to the parent.
       The compiler is told about this with #include <vfork.h>,
       but some compilers (e.g. gcc -O) don't grok <vfork.h>.
       Test for this by using lots of local variables, at least
       as many local variables as main has allocated so far
       including compiler temporaries.  4 locals are enough for
       gcc 1.40.3 on a sparc, but we use 8 to be safe.
       A buggy compiler should reuse the register of parent
       for one of the local variables, since it will think that
       parent can't possibly be used any more in this routine.
       Assigning to the local variable will thus munge parent
       in the parent process.  */
    pid_t
      p = getpid(), p1 = getpid(), p2 = getpid(), p3 = getpid(),
      p4 = getpid(), p5 = getpid(), p6 = getpid(), p7 = getpid();
    /* Convince the compiler that p..p7 are live; otherwise, it might
       use the same hardware register for all 8 local variables.  */
    if (p != p1 || p != p2 || p != p3 || p != p4
	|| p != p5 || p != p6 || p != p7)
      _exit(1);

    /* On some systems (e.g. SunOS 5.2), if the parent is catching
       a signal, the child ignores the signal before execing,
       and the parent later receives that signal, the parent dumps core.
       Test for this by ignoring SIGINT in the child.  */
    signal (SIGINT, SIG_IGN);

    /* On some systems (e.g. IRIX 3.3),
       vfork doesn't separate parent from child file descriptors.
       If the child closes a descriptor before it execs or exits,
       this munges the parent's descriptor as well.
       Test for this by closing stdout in the child.  */
    _exit(close(fileno(stdout)) != 0);
  } else {
    int status;
    struct stat st;

    while (wait(&status) != child)
      ;
    exit(
	 /* Was there some problem with vforking?  */
	 child < 0

	 /* Did the child fail?  (This shouldn't happen.)  */
	 || status

	 /* Did the vfork/compiler bug occur?  */
	 || parent != getpid()

	 /* Did the signal handling bug occur?  */
	 || kill(parent, SIGINT) != 0
	 || signalled != 1

	 /* Did the file descriptor bug occur?  */
	 || fstat(fileno(stdout), &st) != 0
	 );
  }
}], , AC_DEFINE(vfork, fork))
])dnl
dnl
define(AC_WAIT3,
[echo checking for wait3 that fills in rusage
AC_TEST_PROGRAM([#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
/* HP-UX has wait3 but does not fill in rusage at all.  */
main() {
  struct rusage r;
  int i;
  /* Use a field that we can force nonzero --
     voluntary context switches.
     For systems like NeXT and OSF/1 that don't set it,
     also use the system CPU time.  */
  r.ru_nvcsw = 0;
  r.ru_stime.tv_sec = 0;
  r.ru_stime.tv_usec = 0;
  switch (fork()) {
  case 0: /* Child.  */
    sleep(1); /* Give up the CPU.  */
    _exit(0);
  case -1: _exit(0); /* What can we do?  */
  default: /* Parent.  */
    wait3(&i, 0, &r);
    exit(r.ru_nvcsw == 0
	 && r.ru_stime.tv_sec == 0 && r.ru_stime.tv_usec == 0);
  }
}], AC_DEFINE(HAVE_WAIT3))
])dnl
dnl
define(AC_ALLOCA,
[# The Ultrix 4.2 mips builtin alloca declared by alloca.h only works
# for constant arguments.  Useless!
AC_COMPILE_CHECK(working alloca.h, [#include <alloca.h>],
  [char *p = alloca(2 * sizeof(int));], AC_DEFINE(HAVE_ALLOCA_H))
decl="#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#if HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif
"
AC_COMPILE_CHECK([alloca], $decl,
[char *p = (char *) alloca(1);], , [dnl
alloca_missing=1
AC_PROGRAM_EGREP(winnitude, [
#if defined(CRAY) && ! defined(CRAY2)
winnitude
#else
lossage
#endif
],
AC_FUNC_CHECK([_getb67],AC_DEFINE([CRAY_STACKSEG_END],[_getb67]),
AC_FUNC_CHECK([GETB67],AC_DEFINE([CRAY_STACKSEG_END],[GETB67]),
AC_FUNC_CHECK([getb67],AC_DEFINE([CRAY_STACKSEG_END],[getb67])))))
])
if test -n "$alloca_missing"; then
  # The SVR3 libPW and SVR4 libucb both contain incompatible functions
  # that cause trouble.  Some versions do not even contain alloca or
  # contain a buggy version.  If you still want to use their alloca,
  # use ar to extract alloca.o from them instead of compiling alloca.c.
  ALLOCA=alloca.o
  AC_DEFINE(C_ALLOCA)

  echo 'checking stack direction for C alloca'
  AC_TEST_PROGRAM([find_stack_direction ()
{
  static char *addr = 0;
  auto char dummy;
  if (addr == 0)
    {
      addr = &dummy;
      return find_stack_direction ();
    }
  else
    return (&dummy > addr) ? 1 : -1;
}
main ()
{
  exit (find_stack_direction() < 0);
}], dnl
AC_DEFINE(STACK_DIRECTION,1), AC_DEFINE(STACK_DIRECTION,-1), dnl
AC_DEFINE(STACK_DIRECTION,0))
fi
AC_SUBST(ALLOCA)dnl
])dnl
dnl
define(AC_GETLOADAVG,
[# Some definitions of getloadavg require that the program be installed setgid.
AC_SUBST(NEED_SETGID)NEED_SETGID=false
need_func=true

# Check for the 4.4BSD definition of getloadavg.
AC_HAVE_LIBRARY(util, AC_HAVE_LIBRARY(kvm,
LIBS="$LIBS -lutil -lkvm" need_func=false))

if $need_func; then
# There is a commonly available library for RS/6000 AIX.
# Since it is not a standard part of AIX, it might be installed locally.
LIBS_old="$LIBS"
LIBS="-L/usr/local/lib $LIBS"
AC_HAVE_LIBRARY(getloadavg, LIBS="$LIBS -lgetloadavg" need_func=false,
	LIBS="$LIBS_old")
fi

# Make sure it is really in the library, if we think we found it at all.
AC_REPLACE_FUNCS(getloadavg)

case "$LIBOBJS" in
*getloadavg*)
need_func=true
AC_HEADER_CHECK(sys/dg_sys_info.h, [dnl
AC_DEFINE(DGUX) need_func=false
# Some versions of DGUX need -ldgc for dg_sys_info.
AC_HAVE_LIBRARY(dgc)])
if $need_func; then
# We cannot check for <dwarf.h>, because Solaris 2 does not use dwarf (it
# uses stabs), but it's still SVR4.  We cannot check for <elf.h> because
# Irix 4.0.5F has the header but not the library.
AC_HAVE_LIBRARY(elf, AC_DEFINE(SVR4) LIBS="$LIBS -lelf" need_func=false
  AC_HAVE_LIBRARY(kvm, LIBS="$LIBS -lkvm"))
fi
if $need_func; then
AC_HEADER_CHECK(inq_stats/cpustats.h, AC_DEFINE(UMAX4_3) AC_DEFINE(UMAX)
  need_func=false)
fi
if $need_func; then
AC_HEADER_CHECK(sys/cpustats.h, AC_DEFINE(UMAX) need_func=false)
fi
if $need_func; then
AC_HAVE_HEADERS(mach/mach.h)
fi

AC_HEADER_CHECK(nlist.h,
[AC_DEFINE(NLIST_STRUCT)
AC_COMPILE_CHECK(n_un in struct nlist, [#include <nlist.h>],
[struct nlist n; n.n_un.n_name = 0;],
AC_DEFINE(NLIST_NAME_UNION))])dnl

# Figure out whether we will need to install setgid.
AC_PROGRAM_EGREP([Yowza Am I SETGID yet], [dnl
#include "${srcdir}/getloadavg.c"
#ifdef LDAV_PRIVILEGED
Yowza Am I SETGID yet
#endif], [AC_DEFINE(GETLOADAVG_PRIVILEGED) NEED_SETGID=true])dnl
;;

*) AC_DEFINE(HAVE_GETLOADAVG) ;;
esac

if $NEED_SETGID; then
  AC_SUBST(KMEM_GROUP)# Figure out what group owns /dev/kmem.
  # The installed program will need to be setgid and owned by that group.
changequote(,)dnl
  # On Solaris, /dev/kmem is a symlink.  Get info on the real file.
  ls_output=`ls -lgL /dev/kmem 2>${DEV_NULL}`
  # If we got an error (system does not support symlinks), try without -L.
  test -z "$ls_output" && ls_output=`ls -lg /dev/kmem`
  KMEM_GROUP=`echo $ls_output \
    | sed -ne 's/[ 	][ 	]*/ /g;
	       s/^.[sSrwx-]* *[0-9]* *\([^0-9]*\)  *.*/\1/;
	       / /s/.* //;p;'`
changequote([,])dnl
fi
])dnl
dnl
define(AC_UTIME_NULL,
[echo checking utime with null argument
rm -f conftestdata; > conftestdata
# Sequent interprets utime(file, 0) to mean use start of epoch.  Wrong.
AC_TEST_PROGRAM([#include <sys/types.h>
#include <sys/stat.h>
main() {
struct stat s, t;
exit(!(stat ("conftestdata", &s) == 0 && utime("conftestdata", (long *)0) == 0
&& stat("conftestdata", &t) == 0 && t.st_mtime >= s.st_mtime
&& t.st_mtime - s.st_mtime < 120));
}], AC_DEFINE(HAVE_UTIME_NULL))
rm -f core
])dnl
dnl
define(AC_STRCOLL, [echo checking for strcoll
AC_TEST_PROGRAM([#include <string.h>
main ()
{
  exit (strcoll ("abc", "def") >= 0 ||
	strcoll ("ABC", "DEF") >= 0 ||
	strcoll ("123", "456") >= 0);
}], AC_DEFINE(HAVE_STRCOLL))])dnl
dnl
define(AC_SETVBUF_REVERSED,
[AC_TEST_PROGRAM([#include <stdio.h>
/* If setvbuf has the reversed format, exit 0. */
main () {
  /* This call has the arguments reversed.
     A reversed system may check and see that the address of main
     is not _IOLBF, _IONBF, or _IOFBF, and return nonzero.  */
  if (setvbuf(stdout, _IOLBF, (char *) main, BUFSIZ) != 0)
    exit(1);
  putc('\r', stdout);
  exit(0);			/* Non-reversed systems segv here.  */
}], AC_DEFINE(SETVBUF_REVERSED))
rm -f core
])dnl
dnl
dnl
dnl checks for structure members
dnl
dnl
define(AC_STRUCT_TM,
[AC_PROVIDE([$0])AC_COMPILE_CHECK([struct tm in time.h],
[#include <sys/types.h>
#include <time.h>],
[struct tm *tp; tp->tm_sec;], , AC_DEFINE(TM_IN_SYS_TIME))])dnl
dnl
define(AC_TIME_WITH_SYS_TIME,
[AC_COMPILE_CHECK([whether time.h and sys/time.h may both be included],
[#include <sys/types.h>
#include <sys/time.h>
#include <time.h>],
[struct tm *tp;], AC_DEFINE(TIME_WITH_SYS_TIME))])dnl
dnl
define(AC_TIMEZONE,
[AC_REQUIRE([AC_STRUCT_TM])decl='#include <sys/types.h>
'
case "$DEFS" in
  *TM_IN_SYS_TIME*) decl="$decl
#include <sys/time.h>
" ;;
  *) decl="$decl
#include <time.h>
" ;;
esac
AC_COMPILE_CHECK([tm_zone in struct tm], $decl,
[struct tm tm; tm.tm_zone;], AC_DEFINE(HAVE_TM_ZONE), no_tm_zone=1)
if test -n "$no_tm_zone"; then
AC_COMPILE_CHECK(tzname, changequote(<<,>>)dnl
<<#include <time.h>
#ifndef tzname /* For SGI.  */
extern char *tzname[]; /* RS6000 and others want it this way.  */
#endif>>, changequote([,])dnl
[atoi(*tzname);], AC_DEFINE(HAVE_TZNAME))
fi
])dnl
dnl
define(AC_ST_BLOCKS,
[AC_COMPILE_CHECK([st_blocks in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_blocks;],
AC_DEFINE(HAVE_ST_BLOCKS), LIBOBJS="$LIBOBJS fileblocks.o")dnl
AC_SUBST(LIBOBJS)dnl
])dnl
dnl
define(AC_ST_BLKSIZE,
[AC_COMPILE_CHECK([st_blksize in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_blksize;],
AC_DEFINE(HAVE_ST_BLKSIZE))])dnl
dnl
define(AC_ST_RDEV,
[AC_COMPILE_CHECK([st_rdev in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_rdev;],
AC_DEFINE(HAVE_ST_RDEV))])dnl
dnl
dnl
dnl checks for compiler characteristics
dnl
dnl
define(AC_CROSS_CHECK,
[AC_PROVIDE([$0])echo checking whether cross-compiling
# If we cannot run a trivial program, we must be cross compiling.
AC_TEST_PROGRAM([main(){exit(0);}], , cross_compiling=1)
])dnl
dnl
define(AC_CHAR_UNSIGNED,
[echo checking for unsigned characters
AC_TEST_PROGRAM(
[/* volatile prevents gcc2 from optimizing the test away on sparcs.  */
#if !__STDC__
#define volatile
#endif
main() {
#ifdef __CHAR_UNSIGNED__
  exit(1); /* No need to redefine it.  */
#else
  volatile char c = 255; exit(c < 0);
#endif
}], AC_DEFINE(__CHAR_UNSIGNED__))]
)dnl
dnl
define(AC_LONG_DOUBLE,
[AC_REQUIRE([AC_PROG_CC])dnl
echo checking for long double
if test -n "$GCC"; then
AC_DEFINE(HAVE_LONG_DOUBLE)
else
AC_TEST_PROGRAM([int main() {
/* On Ultrix 4.3 cc, long double is 4 and double is 8.  */
exit(sizeof(long double) < sizeof(double)); }],
AC_DEFINE(HAVE_LONG_DOUBLE))
fi
])dnl
dnl
define(AC_INT_16_BITS,
[echo checking integer size
AC_TEST_PROGRAM([main() { exit(sizeof(int) != 2); }],
 AC_DEFINE(INT_16_BITS))
])dnl
dnl
define(AC_LONG_64_BITS,
[echo checking for 64-bit long ints
AC_TEST_PROGRAM([main() { exit(sizeof(long int) != 8); }],
 AC_DEFINE(LONG_64_BITS))
])dnl
dnl
define(AC_WORDS_BIGENDIAN,
[echo checking byte ordering
AC_TEST_PROGRAM([main () {
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  exit (u.c[sizeof (long) - 1] == 1);
}], , AC_DEFINE(WORDS_BIGENDIAN))
])dnl
dnl
define(AC_ARG_ARRAY,
[echo checking whether the address of an argument can be used as an array
AC_TEST_PROGRAM([main() {
/* Return 0 iff arg arrays are ok.  */
exit(!x(1, 2, 3, 4));
}
x(a, b, c, d) {
  return y(a, &b);
}
/* Return 1 iff arg arrays are ok.  */
y(a, b) int *b; {
  return a == 1 && b[0] == 2 && b[1] == 3 && b[2] == 4;
}], , AC_DEFINE(NO_ARG_ARRAY))
rm -f core
])dnl
dnl
define(AC_INLINE,
[AC_REQUIRE([AC_PROG_CC])if test -n "$GCC"; then
AC_COMPILE_CHECK([inline], , [} inline foo() {], , AC_DEFINE(inline, __inline))
fi
])dnl
define(AC_CONST,
[changequote(,)dnl
dnl Do not put single quotes in the C program text!!
prog='/* Ultrix mips cc rejects this.  */
typedef int charset[2]; const charset x;
/* SunOS 4.1.1 cc rejects this.  */
char const *const *ccp;
char **p;
/* AIX XL C 1.02.0.0 rejects this.
   It does not let you subtract one const X* pointer from another in an arm
   of an if-expression whose if-part is not a constant expression */
const char *g = "string";
ccp = &g + (g ? g-g : 0);
/* HPUX 7.0 cc rejects these. */
++ccp;
p = (char**) ccp;
ccp = (char const *const *) p;
{ /* SCO 3.2v4 cc rejects this.  */
  char *t;
  char const *s = 0 ? (char *) 0 : (char const *) 0;

  *t++ = 0;
}
{ /* Someone thinks the Sun supposedly-ANSI compiler will reject this.  */
  int x[] = {25,17};
  const int *foo = &x[0];
  ++foo;
}
{ /* Sun SC1.0 ANSI compiler rejects this -- but not the above. */
  typedef const int *iptr;
  iptr p = 0;
  ++p;
}
{ /* AIX XL C 1.02.0.0 rejects this saying
     "k.c", line 2.27: 1506-025 (S) Operand must be a modifiable lvalue. */
  struct s { int j; const int *ap[3]; };
  struct s *b; b->j = 5;
}
{ /* ULTRIX-32 V3.1 (Rev 9) vcc rejects this */
  const int foo = 10;
}'
changequote([,])dnl
AC_COMPILE_CHECK([dnl Do not "break" this again.
lack of working const], , [$prog], , AC_DEFINE(const,))])dnl
dnl
dnl
dnl checks for operating system services
dnl
dnl
define(AC_HAVE_POUNDBANG, [dnl
echo "checking if \`#!' works in shell scripts"
echo '#!/bin/cat
exit 69
' > conftest
chmod u+x conftest
(SHELL=/bin/sh; export SHELL; ./conftest > ${DEV_NULL})
if test $? -ne 69; then
   :; $1
else
   :; $2
fi
rm -f conftest
])dnl
define(AC_REMOTE_TAPE,
[echo checking for remote tape and socket header files
AC_HEADER_CHECK(sys/mtio.h, AC_DEFINE(HAVE_SYS_MTIO_H) have_mtio=1)
if test -n "$have_mtio"; then
AC_TEST_CPP([#include <sgtty.h>
#include <sys/socket.h>], PROGS="$PROGS rmt")
fi
])dnl
dnl
define(AC_LONG_FILE_NAMES,
[echo checking for long file names
some_dir_failed=false
# Test for long file names in all the places we know might matter:
#      .		the current directory, where building will happen
#      /tmp		where it might want to write temporary files
#      /var/tmp		likewise
#      /usr/tmp		likewise
#      $prefix/lib	where we will be installing things
#      $exec_prefix/lib	likewise
# eval it to expand exec_prefix.
for dir in `eval echo . /tmp /var/tmp /usr/tmp $prefix/lib $exec_prefix/lib` ; do
  test -d $dir || continue
  test -w $dir || continue # It's less confusing to not echo anything here.
  (echo 1 > $dir/conftest9012345) 2>${DEV_NULL}
  (echo 2 > $dir/conftest9012346) 2>${DEV_NULL}
  val=`${CAT} $dir/conftest9012345 2>${DEV_NULL}`
  test -f $dir/conftest9012345 && test "$val" = 1 || some_dir_failed=true
  rm -f $dir/conftest9012345 $dir/conftest9012346 2> ${DEV_NULL}
done
$some_dir_failed || AC_DEFINE(HAVE_LONG_FILE_NAMES)
])dnl
dnl
define(AC_RESTARTABLE_SYSCALLS,
[echo checking for restartable system calls
AC_TEST_PROGRAM(
[/* Exit 0 (true) if wait returns something other than -1,
   i.e. the pid of the child, which means that wait was restarted
   after getting the signal.  */
#include <sys/types.h>
#include <signal.h>
ucatch (isig) { }
main () {
  int i = fork (), status;
  if (i == 0) { sleep (3); kill (getppid (), SIGINT); sleep (3); exit (0); }
  signal (SIGINT, ucatch);
  status = wait(&i);
  if (status == -1) wait(&i);
  exit (status == -1);
}
], AC_DEFINE(HAVE_RESTARTABLE_SYSCALLS))
])dnl
dnl
define(AC_FIND_X,
[# If we find X, set shell vars x_includes and x_libraries to the paths.
no_x=true
AC_FIND_X_XMKMF
if test -z "$im_usrlibdir"; then
AC_FIND_X_DIRECT
fi
if test -n "$verbose"; then
  test -n "$x_includes" && echo "	found X11 headers in $x_includes"
  test -n "$x_libraries" && echo "	found X11 libraries in $x_libraries"
fi])dnl
dnl
dnl Internal subroutine of AC_FIND_X.
define(AC_FIND_X_XMKMF,
[echo checking for X include and library files with xmkmf
rm -fr conftestdir
if mkdir conftestdir; then
  cd conftestdir
  ${CAT} > Imakefile <<\EOF
acfindx:
	@echo "im_incroot=$(INCROOT); im_usrlibdir=$(USRLIBDIR); im_libdir=$(LIBDIR)"
EOF
  if (xmkmf) >${DEV_NULL} 2>${DEV_NULL} && test -f Makefile; then
    no_x=
    # GNU make sometimes prints "make[1]: Entering...", which would confuse us.
    eval `make acfindx | grep -v make`
    # Open Windows xmkmf reportedly sets LIBDIR instead of USRLIBDIR.
    if test ! -f $im_usrlibdir/libX11.a && test -f $im_libdir/libX11.a; then
      im_usrlibdir=$im_libdir
    fi
    case "$im_incroot" in
	/usr/include) ;;
	*) x_includes="$im_incroot" ;;
    esac
    case "$im_usrlibdir" in
	/usr/lib | /lib) ;;
	*) x_libraries="$im_usrlibdir" ;;
    esac
  fi
  cd ..
  rm -fr conftestdir
fi
])dnl
dnl
dnl Internal subroutine of AC_FIND_X.
define(AC_FIND_X_DIRECT,
[echo checking for X include and library files directly
AC_TEST_CPP([#include <X11/Intrinsic.h>], no_x=,
  for dir in \
    /usr/local/include \
    /usr/unsupported/include \
    /usr/x386/include \
    /usr/local/x11r5/include \
    /usr/include/X11R5 \
    /usr/include/X11R4 \
    /usr/X11R5/include \
    /usr/X11/include \
    /usr/openwin/include \
    /usr/openwin/share/include \
    /usr/lpp/Xamples/include \
    ; \
  do
    if test -r $dir/X11/Intrinsic.h; then
      x_includes=$dir; no_x=
      break
    fi
  done)

# Check for the libraries.  First see if replacing the `include' by
# `lib' works.
AC_HAVE_LIBRARY(Xt, no_x=,
for dir in `echo "$x_includes" | sed s/include/lib/` \
  /usr/local/lib \
  /usr/unsupported/lib \
  /usr/x386/lib \
  /usr/local/x11r5/lib \
  /usr/lib/X11 \
  /usr/lib/X11R4 \
  /usr/X11R5/lib \
  /usr/X11/lib \
  /usr/openwin/lib \
  /usr/lpp/Xamples/lib \
  ; \
do
  for extension in a so sl; do
    if test -r $dir/libXt.$extension; then
      x_libraries=$dir; no_x=
      break 2
    fi
  done
done)])dnl
dnl
dnl
dnl checks for UNIX variants
dnl
dnl
define(AC_AIX,
[echo checking for AIX
AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])AC_PROGRAM_EGREP(yes,
[#ifdef _AIX
  yes
#endif
], AC_DEFINE(_ALL_SOURCE))
])dnl
dnl
define(AC_MINIX,
[AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])AC_HEADER_CHECK(minix/config.h, MINIX=1)
# The Minix shell can't assign to the same variable on the same line!
if test -n "$MINIX"; then
  AC_DEFINE(_POSIX_SOURCE)
  AC_DEFINE(_POSIX_1_SOURCE, 2)
  AC_DEFINE(_MINIX)
fi
])dnl
dnl
define(AC_ISC_POSIX,
[AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])echo checking for POSIXized ISC
if test -d /etc/conf/kconfig.d &&
  grep _POSIX_VERSION [/usr/include/sys/unistd.h] >${DEV_NULL} 2>&1
then
  ISC=1 # If later tests want to check for ISC.
  AC_DEFINE(_POSIX_SOURCE)
  if test -n "$GCC"; then
    CC="$CC -posix"
  else
    CC="$CC -Xp"
  fi
fi
])dnl
dnl
define(AC_XENIX_DIR,
[AC_REQUIRE([AC_DIR_HEADER])echo checking for Xenix
AC_PROGRAM_EGREP(yes,
[#if defined(M_XENIX) && !defined(M_UNIX)
  yes
#endif
], XENIX=1)
if test -n "$XENIX"; then
  LIBS="$LIBS -lx"
  case "$DEFS" in
  *SYSNDIR*) ;;
  *) LIBS="-ldir $LIBS" ;; # Make sure -ldir precedes any -lx.
  esac
fi
])dnl
dnl
define(AC_SCO_INTL,
[AC_HAVE_LIBRARY(intl, LIBS="$LIBS -lintl")
])dnl
dnl
define(AC_IRIX_SUN,
[AC_HAVE_LIBRARY(sun, LIBS="$LIBS -lsun")
])dnl
dnl
define(AC_DYNIX_SEQ,
[AC_HAVE_LIBRARY(seq, LIBS="$LIBS -lseq")
])dnl
dnl
define(AC_STAT_MACROS_BROKEN,[echo checking for broken stat file mode macros
AC_PROGRAM_EGREP([You lose], [#include <sys/types.h>
#include <sys/stat.h>
#ifdef S_ISBLK
#if S_ISBLK (S_IFDIR)
You lose.
#endif
#ifdef S_IFCHR
#if S_ISBLK (S_IFCHR)
You lose.
#endif
#endif /* S_IFCHR */
#endif /* S_ISBLK */
#ifdef S_ISLNK
#if S_ISLNK (S_IFREG)
You lose.
#endif
#endif /* S_ISLNK */
#ifdef S_ISSOCK
#if S_ISSOCK (S_IFREG)
You lose.
#endif
#endif /* S_ISSOCK */
], AC_DEFINE(STAT_MACROS_BROKEN))])dnl
dnl
