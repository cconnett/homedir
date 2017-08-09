# IBlaze aliases and completions
if [[ $- == *i* ]] ; then
  alias ib='iblaze build'
  alias iblaze=/google/data/ro/teams/iblaze/iblaze
  alias ir='iblaze run'
  alias it='iblaze test'
  alias icov='iblaze coverage --combined_report=html'
  alias bc=build_cleaner

  complete -F _blaze::complete_build_target_wrapper -o nospace b
  complete -F _blaze::complete_build_target_wrapper -o nospace bc
  complete -F _blaze::complete_build_target_wrapper -o nospace ib
  complete -F _blaze::complete_test_target_wrapper -o nospace t
  complete -F _blaze::complete_test_target_wrapper -o nospace it

  _blaze::complete_run_target_wrapper() {
    _blaze::complete_target_wrapper "run"
  }
  complete -F _blaze::complete_run_target_wrapper -o nospace r
  complete -F _blaze::complete_run_target_wrapper -o nospace ir

  _blaze::complete_coverage_target_wrapper() {
    _blaze::complete_target_wrapper "coverage"
  }
  complete -F _blaze::complete_coverage_target_wrapper -o nospace cov
  complete -F _blaze::complete_coverage_target_wrapper -o nospace icov
fi
