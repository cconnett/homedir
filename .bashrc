# This file is sourced by all *interactive* bash shells on startup.  This
# file *should generate no output* or it will break the scp and rcp commands.

export PATH=~/bin:/opt/node/bin:/usr/local/bin:/opt/emacs25/bin:/opt/ghc/bin:/opt/arduino:$PATH
export VISUAL='emacs --no-splash --no-desktop --no-init-file'
export EDITOR="$VISUAL -nw"
export LESS='-S -R -F -X'
export HISTCONTROL=ignoreboth:erasedups
export HISTIGNORE=ls:ll:la:l:cd:pwd:exit:su:df:clear:sl:reset:gd:gdc:gcp:gs:gl:d:c:gap
export HISTSIZE=150000
shopt -s histappend
shopt -s checkwinsize
shopt -s globstar
shopt -s extglob
export PROMPT_COMMAND="history -a  ~/.bash_history"
export PYTHONSTARTUP=~/.pythonrc
export ACK_COLOR_MATCH='red'
export ACK_COLOR_FILENAME='on_cyan'
export ACK_COLOR_LINENO='bold blue'
export GIT_CEILING_DIRECTORIES=$HOME
export GIT_EDITOR=$EDITOR
export GDK_NATIVE_WINDOWS=1 # http://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=4870
export P4DIFF='git diff'

bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

alias sl=ls
alias d="date"
alias c="gcal --with-week-number --starting-day=monday --iso-week-number=yes .; d"
alias ls='ls --color=auto -B'
alias ll="ls -ltr"
alias lla="ll -A"
alias la="ls -A"
alias locate='locate -i'
alias sha3='rhash --sha3-224'
alias gap='git add -p'
alias gcne='git commit --amend --no-edit'
alias gcm='git commit -m'
alias gp='git push'
alias gu='git pull'

function available {
  ! git reflog "$1" 1> /dev/null 2> /dev/null
}

function gitnew {
  branch="$1"
  shift
  if [[ -z "$branch" ]]; then
    echo 1>&2 "Must specify a new branch."
    return 1
  fi
  git branch "$branch" master 2> >(egrep -v "already exists|'-' is not" 1>&2)
  gco "$branch"
  git5 sync 2> /dev/null
}

alias gn=gitnew

function gco {
  function message {
    echo 'gco!WIP on branch' "$1"
  }

  force="$1"
  if [[ "$force" == "-f" ]]; then
    shift
  fi

  branch="$1"
  shift
  if [[ -z "$branch" ]]; then
    branch=-
  fi

  if [[ -n "$(git status --porcelain)" ]]; then
    current="$(git rev-parse --abbrev-ref HEAD)"
    git stash save --include-untracked --keep-index --quiet \
        "$(message $current)"
  fi

  if [[ "$branch" != "-" ]] && available "$branch"; then
    if [[ "$force" == "-f" ]]; then
      echo "Creating new branch $branch."
      gitnew "$branch"
    else
      echo "$branch does not exist. Use -f to create."
      return 1
    fi
  else
    git checkout -m "$branch"
  fi

  branch="$(git rev-parse --abbrev-ref HEAD)"
  stash="$(git stash list --grep "$(message "$branch")" | cut -f1 -d: | head -n1)"
  if [[ -n "$stash" ]]; then
    git reset --hard --quiet &&
      git stash pop --index --quiet "$stash"
  fi
}

function gitsplit {
  # Default to splitting current HEAD commit if no commits given.
  if [[ -z "$@" ]]; then
    commits="$(git rev-parse --abbrev-ref HEAD)"
  else
    commits="$@"
  fi

  current="$(git rev-parse --abbrev-ref HEAD)"
  tracked=$(git5 tracked "$current" | grep "//depot/" | cut -d/ -f4-)

  # Invent a name for the new branch based on the first commit message.
  basename=$(python -c 'import re,sys; print "-".join(re.sub(r"\W", "", word.lower()) for word in sys.argv[1:9])' \
                    $(git log "$commits" -1 --pretty=oneline | cut -d' ' -f 2-))
  name="$basename"
  if ! available "$basename"; then
    n=1
    until $(available "${basename}-${n}"); do
      n=$(($n + 1))
      name="${basename}-${n}"
    done
  fi

  # Switch to the new branch and cherry-pick.
  gco master
  git5 sync
  git checkout -b "$name" || return 1
  git5 track "$tracked" --no-package-check --dir-file-overlap --import-empty
  git cherry-pick "$commits"
  gco "$current"
  gco -
}

alias gc='git clean -i'
alias gcp='git checkout -p'
alias gd='git diff'
alias gdc='git diff --cached'
alias ge='git5 export'
alias gl='git log'
alias gs='git status'
alias gsl='git stash list'
alias gitg='gitg --all >& /dev/null &'
alias gitkk='gitk $(git branch | tr "\n*" "  ")>& /dev/null &'
alias gitb='for k in `git branch | sed s/^..//`; do echo -e `git log -1 --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" $k --`\\t"$k";done | sed "s/minutes ago/mins ago/" | sort'
alias gb=gitb
alias queeg='ssh -XYCA cxc0117@queeg.cs.rit.edu'
alias elvis='ssh -XYCA cxc0117@elvis.cs.rit.edu'
alias doors='ssh -XYCA cxc0117@doors.cs.rit.edu'
alias odb='java -jar /home/chris/bin/ODB.jar'
alias serve='python -m SimpleHTTPServer'
alias math='rlwrap math'
alias emacs="$VISUAL 2> /dev/null"
alias inkscape='inkscape 2> /dev/null'
alias z3='ipython -i -c "from z3 import *"'
alias freecad='~/free-cad-code/bin/FreeCAD &'
alias dbg='/google/data/ro/teams/ads-test-debugger/@dbg'

# function ack {
#   test_flag='--notest'
#   for arg in "$@"; do
#     case "$arg" in
#       --test)
#         test_flag=''
#         ;;
#     esac
#   done
#   command ack "$@" $test_flag
# }
function getack {
  curl https://beyondgrep.com/ack-2.18-single-file > ~/bin/ack
  chmod 0755 ~/bin/ack
}
if [[ ! -x ~/bin/ack ]]; then
  getack
fi

function jump {
  g4d $(hostname -s)-$(whoami)-$(basename $(dirname $(pwd)))-$(git symbolic-ref --short HEAD)-git5
}

function current-git-branch {
  if [[ "$HOME" != "$PWD" ]]; then
    branch=$(git branch 2> /dev/null | grep -e "* " | cut -d"*" -f2)
    if [[ -n $branch ]]; then
      echo $branch
      return
    fi
  fi
}

if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    PROMPT_COMMAND="$PROMPT_COMMAND"';PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] $(pointed-dir)\[\033[01;31m\] $(current-git-branch)\[\033[01;34m\]\n$\[\033[00m\] "'
fi

alias tapp='tap_presubmit -cb sandman,integrate'

function lastlog {
  less /export/hda3/tmp/$(ls -t1 /export/hda3/tmp | grep $1 | grep $2 | head -1)
}

function current-switch-target {
  true
}

function pointed-dir {
  red_target_blue='\\[\\033[01;31m\\]'
  red_target_blue+=$(current-switch-target)
  red_target_blue+='\\[\\033[01;34m\\]'
  echo "$PWD" | \
    sed -e "s!$HOME!~!" | \
    sed -e 's!/google/src/cloud/cjc!/cloud!' | \
    sed -e "s/emacs/${red_target_blue}/"
}

[ -e ~/homedir/at-google.bash ] && source ~/homedir/at-google.bash

if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    PROMPT_COMMAND="$PROMPT_COMMAND"';PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] $(pointed-dir)\[\033[01;31m\] $(current-git-branch)\[\033[01;34m\]\n$\[\033[00m\] "'
fi

# Activate bash-completion. Only run if shell is interactive.
if [[ $- == *i* ]] ; then
  [ -f /etc/bash_completion ] && source /etc/bash_completion
  [ -f ~/homedir/git5.sh ] && source ~/homedir/git5.sh
  [ -f ~/homedir/iblaze.sh ] && source ~/homedir/iblaze.sh
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

function adb() {
  EMU_DEPS=/google/data/ro/teams/mobile_eng_prod/emu/live/google3/
  ANDROID_SDK=${EMU_DEPS}/third_party/java/android/android_sdk_linux/
  EMU_SUPPORT=${EMU_DEPS}/tools/android/emulator/support/
  ANDROID_ADB=${ANDROID_SDK}/platform-tools/adb
  ANDROID_ADB=${ANDROID_ADB} $EMU_SUPPORT/adb.turbo "$@"
}
