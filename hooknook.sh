#!/bin/bash
container=bholt-claret-doc
docker run --name=$container -d bholt/claret-doc tail -f /dev/null

dexec() { docker exec -i $container sh -c "$@"; }

dexec 'mkdir -p ~/.ssh'
dexec 'cat > ~/.ssh/id_rsa; chmod go-r ~/.ssh/id_rsa' < ../../keys/id_rsa
dexec 'cat > ~/.ssh/id_rsa.pub' < ../../keys/id_rsa.pub
dexec "echo 'Host *\nStrictHostKeyChecking no' > ~/.ssh/config"
dexec 'git clone git@github.com:bholt/claret-doc.git'
dexec 'git config --global user.name "Hooknook"'
dexec 'git config --global user.email "<>"'
dexec 'git config --global push.default matching'
dexec 'cd claret-doc; git checkout gh-pages; git checkout master; git worktree add .pages gh-pages'

dexec 'cd claret-doc; make deploy'
