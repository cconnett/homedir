alias bisect=/google/data/ro/teams/tetralight/bin/bisect
alias bs='blaze build //devtools/sandman:sandman'
alias csearch='csearch --context=1'
alias er=/google/data/ro/users/ho/hooper/er
alias freecad='/usr/local/bin/FreeCAD'
alias g3python=/google/data/ro/projects/g3python/g3python
alias kri=/google/data/ro/projects/sandman/kill_registered_instance.par
alias pa='gcert'
alias pubsub='/google/data/ro/buildstatic/projects/goops/pubsub'
alias sandmanh=blaze-bin/devtools/sandman/sandman
alias sbc=/google/data/ro/projects/sandman/sandman_borgcfg.par
alias sgcl2='gcl2 --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
alias sgcl2db='gcl2db -- --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
alias sgcl='gcl --model=/home/build/google3/production/borg/devtools-sandman/library/sandman.model'
alias writeme=/google/data/ro/teams/writeme/writeme
alias spool='/google/data/ro/teams/spool/spool_client_mpm/live/spool.par'
alias onborg='/google/data/ro/projects/smartass/onborg'
alias rudi-cli='/google/data/ro/teams/lsc/bin/rudi-cli'

source ~/homedir/bash/g4s.sh

function lastlog {
  less /export/hda3/tmp/$(ls -t1 /export/hda3/tmp | grep $1 | grep $2 | head -1)
}
