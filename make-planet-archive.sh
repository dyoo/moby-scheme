#!/bin/bash
OLDDIR=`pwd`
git archive --format=tar --prefix=moby/ HEAD | (cd /var/tmp && tar xf -)
cd /var/tmp/moby

cd $OLDDIR