# Steel Bank Common Lisp

Build SBCL from source on Debian-based distros

## Install the dependencies

```sh
apt-get install build-essential sbcl
```

From http://www.sbcl.org/ get the latest stable release

## Expand the tarball

```sh
tar vxjf sbcl*.tar.bz2
```

## Compile the code

```sh
./make.sh --fancy
```

## Install

```sh
./install.sh
```

## Update

After getting the new release and compiled, remove the older version as root:

```sh
rm /usr/local/bin/sbcl /usr/local/share/man/man1/sbcl.1

rm -rfv /usr/local/lib/sbcl/
```

In your local account:

```sh
rm ~/.sbclrc ~/quicklisp.lisp

rm -rfv ~/.cache/common-lisp ~/quicklisp
```
