# This file is sourced by all *interactive* bash shells on startup.  This
# file *should generate no output* or it will break the scp and rcp commands.

export PATH=~/bin:/usr/local/bin:/opt/emacs/bin:$PATH
export EDITOR='emacs -nw --no-splash --no-desktop'
export LESS='-S -R -F -X'

export HISTCONTROL=ignoreboth:erasedups
export HISTIGNORE=ls:ll:la:l:cd:pwd:exit:su:df:clear:sl:reset:gd:gdc:gcp:gs:gl:d:c:gap
export HISTSIZE=150000
shopt -s histappend
shopt -s checkwinsize
#export PROMPT_COMMAND='history -a  ~/.bash_history; history -c; history -r; $PROMPT_COMMAND'
export PROMPT_COMMAND='history -a  ~/.bash_history; $PROMPT_COMMAND'

#export PYTHONPATH=~/Python:$PYTHONPATH
export PYTHONSTARTUP=~/.pythonrc
export ACK_COLOR_MATCH='red'
export ACK_COLOR_FILENAME='on_cyan'
export ACK_COLOR_LINENO='bold blue'
export GIT_CEILING_DIRECTORIES=$HOME
export GIT_EDITOR=$EDITOR
export GDK_NATIVE_WINDOWS=1 # http://debbugs.gnu.org/cgi-bin/bugreport.cgi?bug=4870
export P4DIFF='git diff'

alias sl=ls
alias d="date"
alias c="cal -3; d"
alias ls='ls --color=auto -B'
alias ll="ls -ltr"
alias lla="ll -A"
alias la="ls -A"
alias locate='locate -i'
alias gap='git add -p'
alias gcne='git commit --amend --no-edit'
alias gcm='git commit -m'
alias gco='git checkout -m'
alias gc=gco
alias gcp='git checkout -p'
alias gd='git diff'
alias gdc='git diff --cached'
alias ge='git5 export'
alias gl='git log'
alias gs='git status'
alias getack='curl http://beyondgrep.com/ack-2.14-single-file > ~/bin/ack && chmod 0755 ~/bin/ack'
alias gitg='gitg --all >& /dev/null &'
alias gitkk='gitk $(git branch | tr "\n*" "  ")>& /dev/null &'
alias gitb='for k in `git branch | sed s/^..//`; do echo -e `git log -1 --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" $k --`\\t"$k";done | sort'
alias gb=gitb
alias queeg='ssh -XYCA cxc0117@queeg.cs.rit.edu'
alias elvis='ssh -XYCA cxc0117@elvis.cs.rit.edu'
alias doors='ssh -XYCA cxc0117@doors.cs.rit.edu'
alias odb='java -jar /home/chris/bin/ODB.jar'
alias serve='python -m SimpleHTTPServer'
alias please=sudo
alias math='rlwrap math'
alias emacs='emacs 2> /dev/null'
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

function pointed-dir {
  red_target_blue='\\[\\033[01;31m\\]'
  red_target_blue+=$(current-switch-target)
  red_target_blue+='\\[\\033[01;34m\\]'
  echo "$PWD" | \
    sed -e "s!$HOME!~!" | \
    sed -e 's!/google/src/cloud/cjc!/cloud!' | \
    sed -e "s/emacs/${red_target_blue}/"
}

if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
else
    PROMPT_COMMAND=$PROMPT_COMMAND';PS1="\[\033[01;32m\]\u@\h\[\033[01;34m\] $(pointed-dir)\[\033[01;31m\] $(current-git-branch)\[\033[01;34m\]\n$\[\033[00m\] "'
fi


function tapp {
  tap_presubmit -p sandman -c $(cat .git4_perforce_config/CL) "$@"
  cd "$PWD"
}
function lastlog {
  less /export/hda3/tmp/$(ls -t1 /export/hda3/tmp | grep $1 | grep $2 | head -1)
}

if [[ $(hostname -d) == "cs.rit.edu" ]]; then
    export PATH=/usr/gnu/bin:/opt/csw/bin:/bin:/sbin:/usr/bin:/usr/sbin:$PATH
    export VISUAL=$EDITOR
    alias grep='ggrep --color=auto'
    alias emacs='emacs -nw --no-splash'
elif [[ $(hostname -d) == "nyc.corp.google.com" ]]; then
    export PROD=/bigtable/mix-io/devtools-sandman.dashboard.instances.sandman-dashboard
    export CJC=/bigtable/mix-pb/devtools-sandman-testing.dashboard.instances.cjc-dev-instance
    export EM=/google/src/cloud/cjc/emacs/google3
    alias g3python=/google/data/ro/projects/g3python/g3python
    alias submit='git5 submit --sq --tap-project=sandman'
    alias submit2='git5 submit --sq --tap-project=sandman,integrate'
    alias submitall='git5 submit --sq --tap-project=all'
    alias presubmit='git5 export --sq --tap-project=sandman'
    alias presubmit2='git5 export --sq --tap-project=sandman,sandman_clients'
    alias presubmitall='git5 export --sq --tap-project=all'
    alias pubsub='/google/data/ro/buildstatic/projects/goops/pubsub'
    alias cov='blaze coverage --combined_report=html'
    alias sandman-head=blaze-bin/devtools/sandman/sandman
    alias bs='blaze build //devtools/sandman:sandman'
    alias kri=/google/data/ro/projects/sandman/kill_registered_instance.par
    alias sgcl='gcl --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
    alias sgcl2='gcl2 --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
    alias sgcl2db='gcl2db -- --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
    alias sbc=/google/data/ro/projects/sandman/sandman_borgcfg.par
    alias pa='glogin && prodaccess'
    alias b='blaze build'
    alias t='blaze test'
    alias r='blaze run'
elif [[ $(hostname) == "scruffy" ]]; then
    alias zfslist='zfs list -t filesystem -r mpool'
    alias emacs='emacs -nw --no-splash'
else
    export VISUAL='emacs'
    alias zfslist='ssh scruffy zfs list -t filesystem -r mpool'
    export SAT_PATHS=~/bin/sat/clasp/bin:~/bin/sat/minisat/simp:~/bin/sat/rsat_SAT-Race08_final_bin:~/bin/sat/zchaff64
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
fi

export SWITCH_CLIENT='emacs'
[[ -s "$HOME/g4s.bash" ]] && source "$HOME/g4s.bash"

# Activate bash-completion. Only run if shell is interactive.
if [[ $- == *i* ]] ; then
  [ -f /etc/bash_completion ] && source /etc/bash_completion
  __git_complete gc _git_checkout
  __git_complete gco _git_checkout
  __git_complete gl _git_log
  complete -F _blaze::complete_build_target_wrapper -o nospace b
  complete -F _blaze::complete_test_target_wrapper -o nospace t
  complete -o default -o nospace -F _g4d_bash::g4d_completion g4s

  _blaze::complete_run_target_wrapper() {
    _blaze::complete_target_wrapper "run"
  }
  complete -F _blaze::complete_run_target_wrapper -o nospace r

  _blaze::complete_coverage_target_wrapper() {
    _blaze::complete_target_wrapper "coverage"
  }
  complete -F _blaze::complete_coverage_target_wrapper -o nospace cov
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
