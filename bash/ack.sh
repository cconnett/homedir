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

# `getack` deleted for being inherently unsafe.
