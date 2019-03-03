#!/usr/bin/env bash

# This script build a binary release tarball for Archlinux.
#
# It must be executed in the toplevel directory of the hledger-iadd repo:
#
#   ./release/arch/build_release.sh VERSION
#
# This assumes that a docker image called hledger-iadd that is built from the
# Dockerfile in this directory is available.
#
# The repo must be in unmodified state and the release in question must be
# checked out.

set -euo pipefail
IFS=$'\n\t'

if [ $# -lt 1 ]; then 
    echo "Usage: $0 VERSION"
    exit 1
fi

if [ \! -e hledger-iadd.cabal ]; then
    echo "This script must be executed from the toplevel directory of the hledger-iadd repo."
    echo "(The one with the cabal file)."
    exit 1
fi

VERSION=$1
TMPDIR=$(mktemp -d)
CURDIR=$(pwd)

set -x

# Create a source distribution tarball from the repo and copy it to a temporary
# directory.
cabal new-sdist
cp "./dist-newstyle/sdist/hledger-iadd-${VERSION}.tar.gz" "${TMPDIR}"

# Go to the temporary directory
pushd "${TMPDIR}"

# Unpack the source tarball
tar xf "hledger-iadd-${VERSION}.tar.gz"
cd hledger-iadd-${VERSION}

# Build the binary in a docker image
docker run --rm -v $(pwd):/home/hledger-iadd hledger-iadd

# Docker creates root-owned binaries. Change the owner to be able to work with it.
#
# Tips to avoid this are appreciated!
sudo chown ${USER}:$(id -gn) hledger-iadd

# Pack the whole thing up
tar -cJf "hledger-iadd-${VERSION}-archlinux.tar.xz" hledger-iadd

# Sign it
gpg --armor --detach-sign "hledger-iadd-${VERSION}-archlinux.tar.xz"

# And copy the artifacts back to our original working directory
cp "hledger-iadd-${VERSION}-archlinux.tar.xz" "hledger-iadd-${VERSION}-archlinux.tar.xz.asc" "${CURDIR}/"

# Delete the temporary directory
popd
rm -r "${TMPDIR}"
