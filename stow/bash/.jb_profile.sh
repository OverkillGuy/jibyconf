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

# Pipe-able version of emacsclient, using a temp file
# Use via:
# someprocess | emp
emp () {
    f=$(mktemp)
    cat > $f
    emacsclient $f
    rm -v $f
}

alias calc='emacsclient -nw -a "" -e "(calc)"'
alias magit='emacsclient -nw -a "" -e "(progn (magit-status) (delete-other-windows))"'

alias stowconf='stow --dir ~/dev/conf/jibyconf/stow/ --target ~/ '
alias goconf='cd ~/dev/conf/jibyconf/'
alias jibyconf='cd ~/dev/conf/jibyconf/'

nessyxpic () {
    ssh nessyx 'find ~/storage/dcim/ -iname "*.jpg" -newermt "5 minute ago" -type f' \
	| xargs -I% scp nessyx:% .
}

# Extracts gherkin from file, printing finename/line number.
# show-gherkin tests/*.py
# Specify only one file to show only line number
show-gherkin () {
    egrep -osn '^.*(Given|When|Then|And|But|Scenario|Background|Feature|In order to|As a|I want to|I need to|So that)(.*)' $@ \
	| sed -E 's/^(.*):.*(Given|When|Then|And|But|Scenario|Background|Feature|In order to|As a|I want to|I need to|So that)/\1: \2/' \
	| sed 's/"""//'
}

# Format dates as per RFC 3339
alias rfcdate="date --rfc-3339 seconds | sed -e 's/ /T/' -e 's/\+00:00/Z/'"
# Format date as per ISO 8601
alias isodate="date --rfc-3339 date"

## Create detangled.org org-mode file containing all of args content
## as org-mode tangleable code section
## Useful to turn a folder into litterate programming!
detangle () {
    awk 'BEGIN{RS=">>>>>"} FNR == 1{print "* " FILENAME "\n\n#+BEGIN_SRC conf :tangle " FILENAME}; {print $0 "\n#+END_SRC\n"} ' $@ >detangled.org
}
