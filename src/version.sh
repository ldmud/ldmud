# This file defines a number of shell variables to describe the projects
# version. It is meant to be modified by the bumpversion script.

# The checkin date
# $Format: "version_date=\"$ProjectDate$\""$ 
version_date=$(echo "\$Date: 2005-06-20 19:17:56 $" | sed -e 's/\$Date: \([0-9]*-[0-9]*-[0-9]*\) \([0-9]*:[0-9]*:[0-9]*\).*\$/\1 \2/')

# The checkin revision
# $Format: "version_revision=\"$ProjectVersion$\""$ 
version_revision=$(echo "\$Revision: 0 $" | sed -e 's/\$Revision: \([0-9][0-9]*\) *\$/\1/')

# The version type: dev, stable, maint release
# $Format: "version_type=\"$ReleaseType$\""$ 
version_type="dev"

# The version number information
# $Format: "version_micro=\"$ReleaseMicro$\""$ 
version_micro=710
# $Format: "version_minor=\"$ReleaseMinor$\""$ 
version_minor=3
# $Format: "version_major=\"$ReleaseMajor$\""$ 
version_major=3
