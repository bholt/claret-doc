#!/bin/bash
if [ -d plots ]; then
  if test -n "$(shopt -s nullglob; echo plots/*.pdf)"; then
    for f in plots/*.pdf; do
      pdf2svg $f ${f/.pdf/.svg}
    done
  fi
fi
