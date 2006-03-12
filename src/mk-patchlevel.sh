# Create the patchlevel.h file from patchlevel.h.in

# Create the patchlevel.h from the information in version.sh

. version.sh

sed -e "s/\\\$VersionType\\\$/$version_type/g" \
    -e "s/\\\$VersionMicro\\\$/$version_micro/g" \
    -e "s/\\\$VersionMinor\\\$/$version_minor/g" \
    -e "s/\\\$VersionMajor\\\$/$version_major/g" \
    -e "s/\\\$VersionDate\\\$/$version_date/g" \
    -e "s/\\\$VersionRevision\\\$/$version_revision/g" \
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
