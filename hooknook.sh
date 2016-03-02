#!/bin/bash
container=bholt-claret-doc
dexec() { docker exec -i $container sh -c "$@"; }

# if container doesn't already exist, then create it
if [ $(docker ps | grep $container | wc -l) -eq '0' ]; then

  docker run --name=$container -d bholt/claret-doc tail -f /dev/null

  dexec 'mkdir -p ~/.ssh'
  dexec 'cat > ~/.ssh/id_rsa; chmod go-r ~/.ssh/id_rsa' < ../../keys/id_rsa
  dexec 'cat > ~/.ssh/id_rsa.pub' < ../../keys/id_rsa.pub
  dexec "echo 'Host *\nStrictHostKeyChecking no' > ~/.ssh/config"
  dexec 'git clone git@github.com:bholt/claret-doc.git'
  dexec 'git config --global user.name "Hooknook"'
  dexec 'git config --global user.email "<>"'
  dexec 'git config --global push.default matching'
  dexec 'git clone git@github.com:bholt/gen.git'

  dexec 'cd claret-doc; make deploy'

else # if container exists, then just pull latest and re-run
  
  dexec 'cd claret-doc && git pull && make deploy'
  
fi
