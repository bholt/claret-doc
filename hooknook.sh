#!/bin/bash
container=$(docker run -d bholt/claret-r tail -f /dev/null)

dexec() { docker exec -i $container sh -c "$@"; }

dexec "mkdir -p ~/.ssh"
dexec 'cat > ~/.ssh/id_rsa; chmod go-r ~/.ssh/id_rsa' < ../../keys/id_rsa
dexec 'cat > ~/.ssh/id_rsa.pub' < ../../keys/id_rsa.pub
dexec 'git clone git@github.com:bholt/claret-doc.git'
dexec 'cd claret-doc; make deploy'
