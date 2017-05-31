[ -f /etc/bash_completion.d/git-prompt ] && source /etc/bash_completion.d/git-prompt
__git_complete gc _git_checkout 2> /dev/null
__git_complete gco _git_checkout 2> /dev/null
__git_complete gl _git_log 2> /dev/null
