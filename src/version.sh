# This file defines a number of shell variables to describe the projects
# version. It is meant to be modified by a bumpversion script or manually.

# the release type is either 'release' or 'development'. The type is
# 'release' if the currently used source for the compilation is tagged with a
# 3.* tag. Otherwise it is 'development'.

# when in a git repository, determine these values from it automagically and
# override the defaults
if test -d ../.git && test -x "`which git 2>&1;true`" && git describe --match '3.*' 2>/dev/null; then
TAGNAME=`git describe --match '3.*' |cut -d '-' -f 1`
version_major=`echo $TAGNAME |cut -d '.' -f 1`
version_minor=`echo $TAGNAME |cut -d '.' -f 2`
version_micro=`echo $TAGNAME |cut -d '.' -f 3`
version_patch=`echo $TAGNAME |cut -d '.' -f 4`
version_date=`git log --pretty=format:"%ad" -1 $TAGNAME`
version_tagger=`git log --pretty=format:"%an <%ae>" -1 $TAGNAME`
version_revision=`git describe --match '3.*'`

# if source not tagged with 3.*, it is no release, but development
if $( echo $version_revision | grep --quiet '-' ) ;then
  version_type="dev"
  version_longtype="development"
else
  version_type="rel"
  version_longtype="release"
fi;

# for x.y.z the patch might be empty from above, should then be 0.
if [ -z "$version_patch" ]; then
version_patch=0
fi;
else
# The default / fallback for non-git environments)
version_major=3
version_minor=6
version_micro=8
version_patch=0
version_type="rel"
version_longtype="release"
version_date="Mon Jul 7 22:00:00 2025 +0200"
version_tagger="Gnomi <gnomi@unitopia.de>"
version_revision="3.6.8"
fi;
