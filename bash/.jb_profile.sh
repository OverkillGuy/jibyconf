#!/usr/bin/env bash
#AUTHOR: Jb Doyon<jb@jiby.tech>

alias ll='ls -al --color=auto'
alias fbig="find . -size +128M -type f -exec ls -sSh {} +"

function fnew () {
    FIND_PATH=$1
    shift
    find $FIND_PATH -type f -newermt "$*"
}

export EDITOR="emacsclient"
alias e="emacsclient -nw"
alias ev="emacsclient"
alias emd="emacs --daemon"
alias emk="killall emacs"

emp () {
    f=$(mktemp)
    cat > $f
    emacsclient $f
    rm -v $f
}

alias calc="emacsclient -nw -e '(calc)'"
alias magit='emacsclient -nw -a emacs -e "(progn (magit-status) (delete-other-windows))"'

alias stowconf="stow --dir ~/dev/conf/emacs-conf/ --target ~/ "

nessyxpic () {
    ssh nessyx 'find ~/storage/dcim/ -iname "*.jpg" -newermt "5 minute ago" -type f' \
	| xargs -I% scp nessyx:% .
}
