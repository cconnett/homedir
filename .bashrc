# This file is sourced by all *interactive* bash shells on startup.  This
# file *should generate no output* or it will break the scp and rcp commands.

# colors for ls, etc.
[[ -f /etc/DIR_COLORS ]] && eval `dircolors -b /etc/DIR_COLORS`
alias d="ls --color"
alias ls='ls --color=auto -B'
alias ll="ls --color -ltr"
alias locate='locate -i'
alias less='less -S'
alias getack='curl http://betterthangrep.com/ack-standalone > ~/bin/ack && chmod 0755 ~/bin/ack'

# Change the window title of X terminals
case $TERM in
	xterm*|rxvt|Eterm|eterm)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
		;;
	screen)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
		;;
esac

##uncomment the following to activate bash-completion:
[ -f /etc/bash_completion ] && source /etc/bash_completion

if [ $(uname) = "SunOS" ]; then
    export PATH=/usr/gnu/bin:/opt/csw/bin:/bin:/sbin:/usr/bin:/usr/sbin:$PATH
    #export LD_LIBRARY_PATH="/usr/local/gnu/lib:$LD_LIBRARY_PATH" # Fix python
    export EDITOR='emacs -nw --no-splash'
    export VISUAL=$EDITOR
    alias zfslist='zfs list -t filesystem -r mpool'
    alias emacs='emacs -nw --no-splash'
    alias grep='ggrep --color=auto'
else
    #export LD_LIBRARY_PATH=/usr/lib:$LD_LIBRARY_PATH
    #export PATH=~/compiz/install/bin:$PATH
    alias zfslist='ssh scruffy zfs list -t filesystem -r mpool'
    export EDITOR='emacs -nw --no-splash'
    export VISUAL='emacs'
    export GIT_EDITOR=$EDITOR
    alias emacs='emacs 2> /dev/null'
fi
export SAT_PATHS=~/bin/sat/clasp/bin:~/bin/sat/minisat/simp:~/bin/sat/rsat_SAT-Race08_final_bin:~/bin/sat/zchaff64
export PATH=~/.cabal/bin:~/bin:/usr/local/bin:/home/chris/.gem/ruby/1.9.1/bin:$SAT_PATHS:$PATH

export HISTCONTROL=ignoreboth
export HISTIGNORE=ls:ll:la:l:cd:pwd:exit:su:df:clear

# append to the history file, don't overwrite it
shopt -s histappend
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export PYTHONPATH=~/Python:~/compiz/install/lib64/python2.5/site-packages:$PYTHONPATH
export ACK_OPTIONS='--no-html'
export ACK_COLOR_MATCH='red'
export ACK_COLOR_FILENAME='on_cyan'
export GIT_CEILING_DIRECTORIES=$HOME

if which ghc >/dev/null 2>/dev/null; then
    function hmap { ghc -e "interact ($*)";  }
    function hmapl { hmap  "unlines.($*).lines" ; }
    function hmapw { hmapl "map (unwords.($*).words)" ; }
fi

alias queeg='ssh -XYCA cxc0117@queeg.cs.rit.edu'
alias elvis='ssh -XYCA cxc0117@elvis.cs.rit.edu'
alias doors='ssh -XYCA cxc0117@doors.cs.rit.edu'
alias odb='java -jar /home/chris/bin/ODB.jar'
alias serve='python -m SimpleHTTPServer'
alias please=sudo
alias math='rlwrap math'

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w\[\033[01;31m\]$(if [ $HOME != $PWD ]; then git branch 2> /dev/null | grep -e "* " | cut -d"*" -f2; fi)\[\033[01;34m\]\n$\[\033[00m\] '
                #TODO luke style
#,-[lrenn@mnussbaum-02]-[~]-[0]-[3026]
#`-[:)] %
	fi
else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

# Try to keep environment pollution down, EPA loves us.
unset use_color safe_term match_lhs
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
