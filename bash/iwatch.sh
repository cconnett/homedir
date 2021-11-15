function iwatch {
  unset TRACE FILELIST IWATCH_FILE
  TRACE=$(mktemp --suffix .trace)
  FILELIST=$(mktemp --suffix .list)
  IWATCH_FILE=$(mktemp --suffix .manifest)
  echo $TRACE $FILELIST $IWATCH_FILE
  while true; do
    echo -n "Running '$(echo $@ | cut -c 1-$(( $(tput cols) - 11 )) )…'"
    strace -ff -o $TRACE -- $@
    STATUS=$?
    if [[ $STATUS == 0 ]]; then
      >&2 echo -e "\r\e[2K\e[32mOK\e[0m @ $(date)"
    else
      >&2 echo -e "\r\e[2K\e[31mEXIT $STATUS @ $(date)\e[0m"
    fi
    rm $FILELIST
    touch $FILELIST
    cat $TRACE.* | \
      grep '^open('  | \
      grep -v '= -1 ENOENT' | \
      cut -f2 -d'"' | \
      sort -u | \
      egrep -v '^/proc' | \
      while read line; do
        if [[ -f $line ]]; then
        echo $line >> $FILELIST
      fi
    done
    sha1sum $(cat $FILELIST) > $IWATCH_FILE
    while sha1sum -c --status $IWATCH_FILE || break; do
      inotifywait -q -q -e modify --fromfile $FILELIST
    done
  done
}

function filewatch {
  while true; do
    inotifywait -e modify ** 2> /dev/null > /dev/null
    eval "$@"
  done
}
