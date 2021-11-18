export SWITCH_CLIENT='switch'

function find-parent-google3 {
  g3="$1"
  shift
  until [[ $(basename "$g3") == 'google3' ||
             $(basename "$g3") == '/' ||
             $(basename "$g3") == '.' ]]; do
    g3=$(dirname "$g3")
  done
  if [[ $(basename "$g3") == 'google3' ]]; then
    echo "$g3"
  fi
}

function current-switch-target {
  g3=$(find-parent-google3 "$PWD")
  if [[ -n "$g3" ]]; then
    target=$(cat "$g3/../.citc/target_of_switch_client" 2> /dev/null)
    if [[ -n "$target" ]]; then
      echo "$target"
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
    sed -e "s/${SWITCH_CLIENT}/${red_target_blue}/" | \
    cat
}

function g4s {
  G4_SWITCH="g4 switch"
  if [[ -n "$SWITCH_CLIENT" ]]; then
    G4_SWITCH="$G4_SWITCH -s $SWITCH_CLIENT"
  fi
  force=
  client=
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -f)
        force="$1"
        shift
        ;;
      *)
        client="$1"
        shift
        ;;
    esac
  done

  if [[ "$client" == '-' ]]; then
    if [[ -f ~/.OLDTARGET_${SWITCH_CLIENT} ]]; then
      client=$(cat ~/.OLDTARGET_${SWITCH_CLIENT})
    else
      echo No previous target.
      return
    fi
  fi
  prevtarget="$(current-switch-target)"
  echo -n "$prevtarget" > ~/.OLDTARGET_${SWITCH_CLIENT}
  output=$($G4_SWITCH "$client" 2>&1)
  if [[ $? -ne 0 && -n "$force" ]]; then
    g4d -f "$client"
    output=$($G4_SWITCH "$client" 2>&1)
  else
    if [[ -n "$client" ]]; then
      echo "$output"
    else
      echo "Switch client ${SWITCH_CLIENT} is pointing at $(current-switch-target)."
    fi
  fi
  g4d "$SWITCH_CLIENT"
}

function g4s_completion {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts=$(g4 myclients | fgrep ':citc' | cut -d: -f2)
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]] ; then
 	COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
 	return 0
    fi
}


# if [[ $- == *i* ]] ; then
#     complete -o default -o nospace -F g4s_completion g4s
# fi
