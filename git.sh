[ -f /etc/bash_completion.d/git-prompt ] && source /etc/bash_completion.d/git-prompt
__git_complete gc _git_checkout 2> /dev/null
__git_complete gco _git_checkout 2> /dev/null
__git_complete gl _git_log 2> /dev/null


function gcm {
  git commit -m "$*"
}
function gcam {
  git commit -am "$*"
}
function gce {
  git commit --amend -m "$*"
}
alias gap='git add -p'
alias gcne='git commit --amend --no-edit'
alias gp='git push'
alias gu='git pull'
alias gco='git checkout'
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

function current-git-branch {
  if [[ "$HOME" != "$PWD" ]]; then
    branch=$(git branch 2> /dev/null | grep -e "* " | cut -d"*" -f2)
    if [[ -n $branch ]]; then
      echo $branch
      return
    fi
  fi
}
