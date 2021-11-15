function ack {
  test_flag='--notest'
  for arg in "$@"; do
    case "$arg" in
      --test)
        test_flag=''
        ;;
    esac
  done
  command ack "$@" $test_flag
}

function getack {
  echo 'Downloading ack...'
  curl https://beyondgrep.com/ack-v3.5.0 > ~/bin/ack
  chmod 0755 ~/bin/ack
}
