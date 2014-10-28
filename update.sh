#!/bin/bash

ConfigDir=~/emacs-config
ModulesDir=~/emacs-config/site-lisp
SiteLispDirs=~/emacs-config/site-lisp/*/

for dir in $SiteLispDirs; do
    if [[ -f $dir/.git ]];
    then
	cd $dir
	echo $dir
        git pull
        git submodule update --recursive
	cd $SiteLispDirs
    fi
done

# rm all elc
echo "removing all byte-compiled files."
cd $ConfigDir
find . -iname "*.elc" -delete

# make org-mode
echo "building org-mode"
cd $ModulesDir
cd org-mode
make

# make js2-mode
echo "building js2-mode"
cd $ModulesDir
cd js2-mode
make

# make magit
echo "building magit"
cd $ModulesDir
cd magit
make

# make mu
echo "building mu"
cd $ModulesDir
cd mu
autoconf
./configure
make
cd mu4e
make
cd ..
sudo make install

echo "all done."
cd $OLDPWD
