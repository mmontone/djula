#!/bin/sh

# Publish docs to github pages branch

make html
make latexpdf
cp -r build /tmp/cldomain-docs
git checkout gh-pages
rm -rf build
mkdir build
cp -r /tmp/cldomain-docs/html build
cp -r /tmp/cldomain-docs/latex build
cp -r /tmp/cldomain-docs/doctrees build
rm -rf /tmp/cldomain-docs
git add build
git commit -a
