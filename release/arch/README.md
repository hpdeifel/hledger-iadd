This Dockerfile is used to produce the [binary AUR package](https://aur.archlinux.org/packages/hledger-iadd-bin/) for Archlinux.

# Building the Image

```sh
docker build -t hledger-iadd .
```

# Building the binary from a release tarball

```sh
tar xf hledger-iadd-1.3.8.tar.gz
cd hledger-iadd-1.3.8
docker run --rm -v `pwd`:/home/hledger-iadd hledger-iadd
```

You will then have a binary called `hledger-iadd` in the current working
directory that is possibly owned by root. If so, give it the correct permissions
with `chown ${USER}:${USER}`.

# Building and signing a tarball

This binary is already stripped, but still rather large. Let's compress it:

```sh
tar -cJf hledger-iadd-1.3.8-archlinux.tar.xz hledger-iadd
```

And sign the resulting tarball with gpg:

```sh
gpg --armor --detach-sign hledger-iadd-1.3.8-archlinux.tar.xz
```

This creates a new file `hledger-iadd-1.3.8-archlinux.tar.xz.asc` with the
signature.

# Uploading to Github release

This step is obvious

# Updating the AUR package

Last but not least, bump the version number in the AUR PKGBUILD, update the
checksums (from local files, not github), run `makepkg` and `makepkg
--printsrcinfo > .SRCINFO`, commit and push.
