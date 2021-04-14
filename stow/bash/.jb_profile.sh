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

alias stowconf="stow --dir ~/dev/conf/jibyconf/stow/ --target ~/ "

nessyxpic () {
    ssh nessyx 'find ~/storage/dcim/ -iname "*.jpg" -newermt "5 minute ago" -type f' \
	| xargs -I% scp nessyx:% .
}

# Extracts gherkin from file, printing finename/line number.
# show-gherkin tests/*.py
# Specify only one file to show only line number
show-gherkin () {
    egrep -osn '^.*(Given|When|Then|And|But|Scenario|Background|Feature)(.*)' $@ \
	| sed -E 's/^(.*):.*(Given|When|Then|And|But|Scenario|Background|Feature)/\1: \2/' \
	| sed 's/"""//'
}

# Format dates as per RFC 3339
alias rfcdate="date --rfc-3339 seconds | sed -e 's/ /T/' -e 's/\+00:00/Z/'"
# Format date as per ISO 8601
alias isodate="date --rfc-3339 date"
