# This file defines a number of shell variables to describe the projects
# version. It is meant to be modified by the bumpversion script.

# The checkin date
version_date=$(echo "\$Date$" | sed -e 's/[$]Date: \([0-9]*-[0-9]*-[0-9]*\).*\$/\1/')

# The checkin time
version_time=$(echo "\$Date$" | sed -e 's/[$]Date: \([0-9]*-[0-9]*-[0-9]*\) \([0-9]*:[0-9]*:[0-9]*\).*\$/\2/')

# The checkin revision
version_revision=$(echo "\$Revision$" | sed -e 's/[$]Revision: \([0-9][0-9]*\) *\$/\1/')

# The version type: dev, stable, maintenance, release
version_type="dev"
version_longtype="development"

# A timestamp, to be used by bumpversion and other scripts.
# It can be used, for example, to 'touch' this file on every build, thus
# forcing revision control systems to add it on every checkin automatically.
version_stamp="Sat Oct  6 22:01:23 MDT 2007"

# The version number information
version_micro=715
version_minor=3
version_major=3
