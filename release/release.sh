#!/bin/bash

# This script is for maintainers only! It creates a new release.
#
# Usage:  release.sh VERSION

set -e

if [ -z "$1" ] || [ "$1" = -h ]; then
    echo "$0 VERSION"
    exit 0
fi

version="$1"

if git status --porcelain | grep '^ M' ; then
    echo "You have unstaged changes!" >&2
    exit 1
fi

if [ ! -e .git/ ]; then
    echo "Please run this script from the top-level directory." >&2
    exit 1
fi

########################
# Function definitions #
########################

function run_tests {
    echo "### Running Tests"
    cabal test -O0 -v0
}

function update_changelog {
    echo "### Updating version in ChangeLog.md"
    local headline="# $version  [$(date +%Y-%m-%d)]"
    sed -i "s/^# NEXT VERSION/$headline/" ChangeLog.md
}

function update_cabal_file {
    echo "### Updating version in cabal file"
    local line="^version:\(\W\+\).*"
    sed -i "s/${line}/version:\1${version}/" hledger-iadd.cabal
}

function make_commit {
    echo "### Comitting"
    git add hledger-iadd.cabal
    git add ChangeLog.md
    git commit -m "Bump version and update ChangeLog"
    git tag -s -m "Version $version" v${version}
}

function push_changes {
    echo -e "### Pushing\n"
    echo -n "really push? (return to confirm) "
    read
    git push
    git push --tags
}

function make_hackage_tarball {
    echo "### Creating the hackage tarball"
    cabal sdist -o . hledger-iadd.cabal
}

function check_hackage_tarball {
    echo "### Testing the hackage tarball"
    local tmpdir="$(mktemp -d)"

    tar xf "hledger-iadd-${version}.tar.gz" --directory=${tmpdir}
    
    pushd "$tmpdir/hledger-iadd-${version}/"
    cabal test -O0 -v0
    popd
}

function upload_hackage_tarball {
    echo -e "### Uploading the hackage tarball\n"
    echo -n "Really upload? (return to confirm) "
    read
    cabal upload "hledger-iadd-${version}.tar.gz"
}

function next_steps {
    echo -e "### Manual steps required:\n"
    echo " - [ ] Create github release from tag"
    echo " - [ ] Publish the hackage release"
    echo " - [ ] Make static binary"
    echo " - [ ] Update AUR Package"
    echo " - [ ] Announce release"
}

###############
# Main script #
###############

run_tests; echo
update_changelog; echo
update_cabal_file; echo
run_tests; echo
make_commit; echo
push_changes; echo
make_hackage_tarball; echo
check_hackage_tarball; echo
upload_hackage_tarball; echo
next_steps
