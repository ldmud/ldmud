#! /bin/sh

# Create the patchlevel.h file from patchlevel.h.in
# Create the patchlevel.h from the information in version.sh

#enforce C locale just to be sure...
export LANG=C

. ./version.sh

sed -e "s/\\\$VersionType\\\$/$version_type/g" \
    -e "s/\\\$VersionLongType\\\$/$version_longtype/g" \
    -e "s/\\\$VersionPatch\\\$/$version_patch/g" \
    -e "s/\\\$VersionMicro\\\$/$version_micro/g" \
    -e "s/\\\$VersionMinor\\\$/$version_minor/g" \
    -e "s/\\\$VersionMajor\\\$/$version_major/g" \
    -e "s/\\\$VersionDate\\\$/$version_date/g" \
    -e "s/\\\$VersionTagger\\\$/$version_tagger/g" \
    -e "s/\\\$VersionRevision\\\$/$version_revision/g" \
    patchlevel.h.in > patchlevel.h.tmp

# Only update patchlevel.h if it really changed.

if diff patchlevel.h.tmp patchlevel.h > /dev/null 2>&1; then
    rm patchlevel.h.tmp
else
    mv patchlevel.h.tmp patchlevel.h
fi

