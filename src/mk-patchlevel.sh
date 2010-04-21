#! /bin/sh

# Create the patchlevel.h file from patchlevel.h.in
# Create the patchlevel.h from the information in version.sh

#enforce C locale just to be sure...
export LANG=C

. ./version.sh

# extract revision information from SCM (svn or git) if available
if test -d ../.git && test -x "`which git 2>&1;true`"; then
  HASH="`git rev-parse --short=16 HEAD`";
elif test -d .svn && test -x "`which svn 2>&1;true`"; then
  HASH="`svn info .. |grep Revision | cut -d ' ' -f 2`";
else
#fall back to version.sh information (hopefully it is created properly by the
#release scripts...)
  HASH=$version_revision
fi;

sed -e "s/\\\$VersionType\\\$/$version_type/g" \
    -e "s/\\\$VersionLongType\\\$/$version_longtype/g" \
    -e "s/\\\$VersionMicro\\\$/$version_micro/g" \
    -e "s/\\\$VersionMinor\\\$/$version_minor/g" \
    -e "s/\\\$VersionMajor\\\$/$version_major/g" \
    -e "s/\\\$VersionDate\\\$/$version_date/g" \
    -e "s/\\\$VersionRevision\\\$/$HASH/g" \
    patchlevel.h.in > patchlevel.h.tmp

# Only update patchlevel.h if it really changed.

if diff patchlevel.h.tmp patchlevel.h > /dev/null 2>&1; then
    rm patchlevel.h.tmp
else
    mv patchlevel.h.tmp patchlevel.h
fi

# 'touch' version.sh, so that svn will check it in again
# This way version.sh will always represent the latest revision number/date.

sed -e "s/version_stamp=\".*\"/version_stamp=\"$(date)\"/g" \
    version.sh > version.sh.out

mv version.sh.out version.sh
