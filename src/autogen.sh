#!/bin/sh
# Skeleton script for auto-generating some files like configure for compiling
# the driver. Which will now check if autoconf is installed first if not, it
# will copy the premade packaged files into place.

if command -v autoconf >/dev/null; then
	cd autoconf

	autoreconf

	mv configure ..
	mv machine.h.in ..

	cd ..	
else
	echo "Autoconf not found: copying premade configure and machine.h.in files from ../autoconf"
	cp ../autoconf/configure .
	cp ../autoconf/machine.h.in .
fi
