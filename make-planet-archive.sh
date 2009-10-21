#!/bin/bash
OLDDIR=`pwd`
mkdir -p tmp
rm -rf tmp/moby
git archive --format=tar --prefix=moby/ HEAD | (cd tmp && tar xf -)
cd tmp/moby
rm -rf sandbox
cd $OLDDIR/tmp
planet create moby
cd $OLDDIR
cp tmp/moby.plt .