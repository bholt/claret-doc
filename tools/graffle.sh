#!/bin/bash
if ! command -v omnigraffle-export >/dev/null 2>&1 ; then
  touch $3
else
  SVG=${3/.pdf/.svg}
	omnigraffle-export -c $2 $1 $3
fi