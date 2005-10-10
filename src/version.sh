# This file defines a number of shell variables to describe the projects
# version. It is meant to be modified by the bumpversion script.

# The checkin date
# $Format: "version_date=\"$ProjectDate$\""$ 
version_date="Mon, 20 Jun 2005 19:42:30 -0600"

# The checkin time
version_time=$(echo "\$Date$" | sed -e 's/\$Date$/\2/')

# The checkin revision
# $Format: "version_revision=$ProjectVersion$"$ 
version_revision=3-3.710

# The version type: dev, stable, maint release
# $Format: "version_type=\"$ReleaseType$\""$ 
version_type="dev"

# The version number information
# $Format: "version_micro=$ReleaseMicro$"$ 
version_micro=710
# $Format: "version_minor=$ReleaseMinor$"$ 
version_minor=3
# $Format: "version_major=$ReleaseMajor$"$ 
version_major=3
