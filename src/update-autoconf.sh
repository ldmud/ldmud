#!/bin/sh
# Skeleton script for auto-generating some files like configure for compiling
# the driver. Which will now check if autoconf is installed first if not, it
# will copy the premade packaged files into place.

cp configure ../autoconf;
cp machine.h.in ../autoconf;
echo "Copied configure and machine.h.in to ../autoconf";
