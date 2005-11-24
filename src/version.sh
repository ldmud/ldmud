# This file defines a number of shell variables to describe the projects
# version. It is meant to be modified by the bumpversion script.

# The checkin date
# $Format: "version_date=\"$ProjectDate$\""$ 
version_date=$(echo "\$Date$" | sed -e 's/[$]Date: \([0-9]*-[0-9]*-[0-9]*\).*\$/\1/')

# The checkin time
version_time=$(echo "\$Date$" | sed -e 's/[$]Date: \([0-9]*-[0-9]*-[0-9]*\) \([0-9]*:[0-9]*:[0-9]*\).*\$/\2/')

# The checkin revision
# $Format: "version_revision=$ProjectVersion$"$ 
version_revision=$(echo "\$Revision$" | sed -e 's/[$]Revision: \([0-9][0-9]*\) *\$/\1/')

# The version type: dev, stable, maint release
# $Format: "version_type=\"$ReleaseType$\""$ 
version_type="dev"

# A timestamp, to be used by bumpversion and other scripts.
# It can be used, for example, to 'touch' this file on every build, thus
# forcing revision control systems to add it on every checkin automatically.
version_stamp="Thu Nov 24 13:18:42 MST 2005"

# The version number information
# $Format: "version_micro=711
version_micro=711
# $Format: "version_minor=3
version_minor=3
# $Format: "version_major=3
version_major=3
