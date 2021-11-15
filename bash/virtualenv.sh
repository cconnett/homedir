# virtualenv and virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
source /usr/share/virtualenvwrapper/virtualenvwrapper.sh

function virtual-env {
  if [[ -n $VIRTUAL_ENV ]]; then
    echo "\[\033[01;39m\]($(basename $VIRTUAL_ENV))\[\033[01;39m\] "
  else
    echo ""
  fi
}
