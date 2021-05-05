#!/bin/sh

# Build SBCL from source on Debian-based distros

# Install the dependencies
apt-get install build-essential sbcl

# From http://www.sbcl.org/ get the latest stable release

# Expand the tarball:
tar vxjf sbcl*.tar.bz2

# Compile the code
./make.sh --fancy
