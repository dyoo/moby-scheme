#!/bin/bash
MAJOR=3  # `perl -ne 'if (/define MAJOR \"(.*)\"/) { print $1} ' src/compiler/version.ss`
MINOR=7  #`perl -ne 'if (/define MINOR \"(.*)\"/) { print $1} ' src/compiler/version.ss`


OLDDIR=`pwd`
mkdir -p tmp
rm -rf tmp/moby
git archive --format=tar --prefix=moby/ HEAD | (cd tmp && tar xf -)

cd $OLDDIR/tmp/moby
rm -rf sandbox


cd $OLDDIR/tmp

planet unlink dyoo moby.plt $MAJOR $MINOR
planet link dyoo moby.plt $MAJOR $MINOR moby

planet create moby

planet unlink dyoo moby.plt $MAJOR $MINOR

cd $OLDDIR
cp tmp/moby.plt .