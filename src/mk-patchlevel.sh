# Create the patchlevel.h file from patchlevel.h.in

. version.sh

sed -e "s/\\\$VersionType\\\$/$version_type/g" \
    -e "s/\\\$VersionMicro\\\$/$version_micro/g" \
    -e "s/\\\$VersionMinor\\\$/$version_minor/g" \
    -e "s/\\\$VersionMajor\\\$/$version_major/g" \
    -e "s/\\\$VersionDate\\\$/$version_date/g" \
    -e "s/\\\$VersionRevision\\\$/$version_revision/g" \
    patchlevel.h.in > patchlevel.h
