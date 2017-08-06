# autogen.sh
# usage: sh -ex autogen.sh

test -d build-aux || mkdir build-aux

actually ()
{
    src=$1
    dst=$2

    test "$dst" || dst=$src

    gnulib-tool --copy-file $src $dst
    rm -f ${dst}~
}
actually build-aux/install-sh
actually doc/INSTALL.UTF-8 INSTALL

autoconf

# autogen.sh ends here
