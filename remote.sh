#!/bin/bash
DIR="${BASH_SOURCE%/*}"
cd $DIR
server=ibex.syslab
dest='/scratch/$USER/claret-doc'
echo ">>> copying files to $server:$dest"
ssh $server mkdir -p $dest
rsync -a . $server:$dest
echo ">>> building remotely"
ssh $server "cd $dest; docker run --rm -i -v $dest:/src bholt/claret-doc $@"
echo ">>> copying files back"
rsync -a $server:$dest ..
