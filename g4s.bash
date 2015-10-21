function find-parent-google3 {
  g3=$PWD
  until [[ $(basename $g3) == "google3" || $g3 == "/" ]]; do
    g3=$(dirname $g3)
  done
  if [[ $g3 != "/" ]]; then
    echo $g3
  fi
}

function current-switch-target {
  g3=$(find-parent-google3)
  if [[ -n $g3 ]]; then
    target=$(cat ${g3}/../.citc/target_of_switch_client 2> /dev/null)
    if [[ -n $target ]]; then
      echo $target
      return
    fi
  fi
}

function g4s {
  G4_SWITCH="g4 switch"
  if [[ -n $SWITCH_CLIENT ]]; then
    G4_SWITCH="$G4_SWITCH -s $SWITCH_CLIENT"
  fi
  force=
  client=
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -f)
        force=$1
        shift
        ;;
      *)
        client=$1
        shift
        ;;
    esac
  done

  if [[ "$client" == '-' ]]; then
    client=$(cat ~/.OLDTARGET_${SWITCH_CLIENT})
  fi

  prevtarget=$(current-switch-target)
  echo -n $prevtarget > ~/.OLDTARGET_${SWITCH_CLIENT}
  output=$($G4_SWITCH $client 2>&1)
  if [[ $? -ne 0 && -n $force ]]; then
    g4d -f $client
    if [[ -n $SWITCH_CLIENT ]]; then
      g4d $SWITCH_CLIENT
    fi
    $G4_SWITCH $client
  else
    echo $output
  fi
}

if [[ $- == *i* ]] ; then
  complete -o default -o nospace -F _g4d_bash::g4d_completion g4s
fi
