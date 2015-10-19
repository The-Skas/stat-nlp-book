#!/bin/bash
#Update Stuff

git pull
git submodule update --init --recursive
sbt compile
cd wolfe; sbt compile; sbt publish-local; cd ..
cp moro/conf/application-statnlpbook.conf moro/conf/application.conf
cd moro; git checkout master; sbt run
ln -s $PWD/src/main/moro/figures $PWD/moro/public/figures

