# IBazel aliases and completions
if [[ $- == *i* ]] ; then
  alias b='bazel build'
  alias bc=build_cleaner
  alias cov='bazel coverage'
  alias ib='ibazel build'
  alias icov='ibazel coverage'
  alias ir='ibazel run'
  alias it='ibazel test'
  alias r='bazel run'
  alias t='bazel test'

  complete -F _bazel::complete_build_target_wrapper -o nospace b
  complete -F _bazel::complete_build_target_wrapper -o nospace bc
  complete -F _bazel::complete_build_target_wrapper -o nospace br
  complete -F _bazel::complete_build_target_wrapper -o nospace ib
  complete -F _bazel::complete_test_target_wrapper -o nospace t
  complete -F _bazel::complete_test_target_wrapper -o nospace it

  _bazel::complete_run_target_wrapper() {
    _bazel::complete_target_wrapper "run"
  }
  complete -F _bazel::complete_run_target_wrapper -o nospace r
  complete -F _bazel::complete_run_target_wrapper -o nospace ir

  _bazel::complete_coverage_target_wrapper() {
    _bazel::complete_target_wrapper "coverage"
  }
  complete -F _bazel::complete_coverage_target_wrapper -o nospace cov
  complete -F _bazel::complete_coverage_target_wrapper -o nospace icov
fi
