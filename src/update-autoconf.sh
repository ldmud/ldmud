#!/bin/sh
# Script for updating the built-in files created by autoconf.
# They are supplied for users without autoconf and updated just before
# a release is tagged.

./autogen.sh
cp configure ../autoconf;
cp machine.h.in ../autoconf;
echo "Copied updated configure and machine.h.in to ../autoconf";

