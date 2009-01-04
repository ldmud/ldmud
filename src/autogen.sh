#!/bin/sh
# Skeleton script for auto-generating some files like configure for compiling
# the driver.

cd autoconf

autoreconf

mv configure ..
mv machine.h.in ..

cd ..

