# This file is sourced by all *interactive* bash shells on startup.  This
# file *should generate no output* or it will break the scp and rcp commands.

export PATH=~/bin:/usr/local/bin:$PATH:/usr/games
export VISUAL='emacs --no-splash --no-desktop'
export EDITOR="$VISUAL -nw"
export LESS='-S -R -F -X'
export HISTCONTROL=ignoreboth:erasedups
export HISTIGNORE='ls:ll:lla:la:l:cd:pwd:exit:su:df:clear:sl:reset:gd:gdc:gcp:gs:gl:d:c:gap:pa:blaze clean'
export HISTSIZE=150000
shopt -s histappend
shopt -s checkwinsize
shopt -s globstar
shopt -s extglob
export ACK_COLOR_MATCH='red'
export ACK_COLOR_FILENAME='on_cyan'
export ACK_COLOR_LINENO='bold blue'
export GIT_CEILING_DIRECTORIES=$HOME
export GIT_EDITOR=$EDITOR
export P4DIFF='git diff'

alias sl=ls
alias d="date +'%a %d %b %Y %H:%M:%S %Z'"
alias c="gcal --with-week-number --starting-day=monday --iso-week-number=yes .; d"
alias ls='ls --color=auto -B'
alias ll="ls -ltr"
alias lla="ll -A"
alias la="ls -A"
alias locate='locate -i'
alias sha3='rhash --sha3-224'
alias m='mathematica 2> /dev/null &'
alias ipython=ipython3
alias roll=rolldice
alias python=python3
alias u="date --utc '+%s ; %a %d %b %Y %T'"
alias pa=gcert

alias odb='java -jar /home/chris/bin/ODB.jar'
alias serve='python -m http.server'
alias math='rlwrap math'
alias emacs="$VISUAL 2> /dev/null"
alias inkscape='inkscape 2> /dev/null'
alias z3py='workon z3; ipython -i -c "from z3 import *"'
alias dbg='/google/data/ro/teams/ads-test-debugger/@dbg'
alias pyfactor=/google/data/ro/teams/youtube-code-health/pyfactor
alias pipup='pip install --upgrade pip'

# Activate submodules. Only run if shell is interactive.
if [[ $- == *i* ]] ; then
  [ -f /etc/bash_completion ] && source /etc/bash_completion
  source ~/homedir/bash/ack.sh
  source ~/homedir/bash/at-google.sh
  source ~/homedir/bash/git.sh
  source ~/homedir/bash/iblaze.sh
  source ~/homedir/bash/iwatch.sh
  source ~/homedir/bash/virtualenv.sh
fi

function prompt_command {
  #history -a  ~/.bash_history
  PS1=""
  if [[ $(type -t virtual-env) == function ]]; then
    PS1+=$(virtual-env)
  fi
  PS1+="\[\033[01;32m\]\u@\h\[\033[01;34m\] "
  if [[ $(type -t pointed-dir) == function ]]; then
    PS1+=$(pointed-dir)
  fi
  PS1+="\[\033[01;31m\] "
  if [[ $(type -t current-git-branch) == function ]]; then
    PS1+=$(current-git-branch)
  fi
  PS1+="\[\033[01;34m\]\n$\[\033[00m\] "

  if [[ -n "$TMUX" ]]; then
    target=$(current-switch-target)
    [[ -n $target ]] && tmux rename-window -t${TMUX_PANE} "$target"
  fi
}
export PROMPT_COMMAND="prompt_command;"


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
