#!/bin/bash
MAJOR=2
MINOR=10

OLDDIR=`pwd`
mkdir -p tmp
rm -rf tmp/moby
git archive --format=tar --prefix=moby/ HEAD | (cd tmp && tar xf -)
cd tmp/moby
rm -rf sandbox
cd $OLDDIR/tmp

planet unlink dyoo moby.plt $MAJOR $MINOR
planet link dyoo moby.plt $MAJOR $MINOR moby

planet create moby

planet unlink dyoo moby.plt $MAJOR $MINOR

cd $OLDDIR
cp tmp/moby.plt .