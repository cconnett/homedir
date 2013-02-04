# This file is sourced by all *interactive* bash shells on startup.  This
# file *should generate no output* or it will break the scp and rcp commands.

if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w\[\033[01;31m\]$(if [ $(basename $(dirname "$PWD")) == "review" ]; then echo " *$(basename "$PWD")"; elif [ "$HOME" != "$PWD" ]; then git branch 2> /dev/null | grep -e "* " | cut -d"*" -f2; fi)\[\033[01;34m\]\n$\[\033[00m\] '
fi

export PATH=~/bin:/usr/local/bin:$PATH
export EDITOR='emacs -nw --no-splash'
export LESS='-S -R -F -X'

export HISTCONTROL=ignoreboth
export HISTIGNORE=ls:ll:la:l:cd:pwd:exit:su:df:clear:sl:reset
export HISTSIZE=15000
shopt -s histappend
shopt -s checkwinsize

export PYTHONPATH=~/Python:$PYTHONPATH
export ACK_OPTIONS='--no-html'
export ACK_COLOR_MATCH='red'
export ACK_COLOR_FILENAME='on_cyan'
export ACK_COLOR_LINENO='bold blue'
export GIT_CEILING_DIRECTORIES=$HOME
export GIT_EDITOR=$EDITOR
export GDK_NATIVE_WINDOWS=1 # http://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=4870

alias sl=ls
alias d="ls --color"
alias ls='ls --color=auto -B'
alias ll="ls -ltr"
alias lla="ll -A"
alias la="lla"
alias locate='locate -i'
alias gap='git add -p'
alias gcm='git commit -m'
alias gcp='git checkout -p'
alias gdc='git diff --cached'
alias getack='curl http://betterthangrep.com/ack-standalone > ~/bin/ack && chmod 0755 ~/bin/ack'
alias gitg='gitg --all >& /dev/null &'
alias gitkk='gitk $(git branch | tr "\n*" "  ")>& /dev/null &'
alias queeg='ssh -XYCA cxc0117@queeg.cs.rit.edu'
alias elvis='ssh -XYCA cxc0117@elvis.cs.rit.edu'
alias doors='ssh -XYCA cxc0117@doors.cs.rit.edu'
alias odb='java -jar /home/chris/bin/ODB.jar'
alias serve='python -m SimpleHTTPServer'
alias please=sudo
alias math='rlwrap math'
alias emacs='emacs 2> /dev/null'

if   [ $(hostname -d) == "cs.rit.edu" ]; then
    export PATH=/usr/gnu/bin:/opt/csw/bin:/bin:/sbin:/usr/bin:/usr/sbin:$PATH
    export VISUAL=$EDITOR
    alias grep='ggrep --color=auto'
    alias emacs='emacs -nw --no-splash'
elif [ $(hostname) == "scruffy" ]; then
    alias zfslist='zfs list -t filesystem -r mpool'
    alias emacs='emacs -nw --no-splash'
else
    export VISUAL='emacs'
    alias zfslist='ssh scruffy zfs list -t filesystem -r mpool'
    export SAT_PATHS=~/bin/sat/clasp/bin:~/bin/sat/minisat/simp:~/bin/sat/rsat_SAT-Race08_final_bin:~/bin/sat/zchaff64
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
fi

##uncomment the following to activate bash-completion:
[ -f /etc/bash_completion ] && source /etc/bash_completion

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
