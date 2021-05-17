#!/bin/sh
if test "$1" = "--all"  
then
    BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    echo "clean.sh called with --all (cleaning base directory and ~/.cache/common-lisp)"
    cd $BASE_DIR
    bash clean.sh
    if [ -d ~/.cache/common-lisp ]; then
        echo "cleaning ~/.cache/common-lisp"
        cd ~/.cache/common-lisp
        bash "$BASE_DIR/clean.sh"
    fi
    if [ -d ~/quicklisp ]; then
	echo "cleaning ~/quicklisp"
	cd ~/quicklisp/
	bash "$BASE_DIR/clean.sh"
    fi
else
find . -name '*.ppcf' -exec rm -v {} \;
find . -name '*.*fsl' -exec rm -v {} \;
find . -name '*.fasl' -exec rm -v {} \;
find . -name '*.fas' -exec rm -v {} \;
find . -name '*.*fasl' -exec rm -v {} \;
find . -name '*.x86f' -exec rm -v {} \;
find . -name '*.*x*fsl' -exec rm -v {} \;
find . -name '*~.lisp' -exec rm -v {} \;
find . -name '*~.asd' -exec rm -v {} \;
find . -name '*~' -exec rm -v {} \;
find . -name '*.elc' -exec rm -v {} \;
find . -name '.#*' -exec rm -v {} \;
find . -name '*.allegro-warnings' -exec rm -v {} \;
find . -name '*.build-report' -exec rm -v {} \;
find . -name '*.64xfasl' -exec rm -v {} \;
rm -rvf .tmp/*
fi
