#!/bin/sh

cd $(dirname $0)

PACKAGE=$(basename $PWD)

mkdir -p translation

# Extract text for translation from source
#xgettext --package-name=$PACKAGE -c --from-code UTF-8 -k_ -kN_ -o translation/$PACKAGE.pot *.lisp

# When using preload-catalog the contents it's neccary to recompile the files with
# the preload-catalog in order to get the updated texts.
touch demo.lisp

# To create a new translation (e.g. for the locale nn)
# cd translation
# msginit -l nn

# Update existing translation
cd translation
#for po in *.po ; do
#    msgmerge -U $po $PACKAGE.pot
#done

# Transform .po files into into binary .mo files read by Lisp
for po in *.po ; do
    mkdir -p ../locale/$(basename $po .po)/LC_MESSAGES
    msgfmt -o ../locale/$(basename $po .po)/LC_MESSAGES/$PACKAGE.mo $po
done
