#!/bin/bash

SiteLisp=~/emacs-config/site-lisp/*/

for dir in $SiteLisp; do
    if [[ -f $dir/.git ]];
    then
	cd $dir
	echo $dir
	git reset --hard HEAD
	cd $SiteLisp
    fi
done

cd $OLDPWD