#!/bin/bash
if [ -d plots ]; then
  for f in plots/*.pdf; do
    pdf2svg $f ${f/.pdf/.svg}
  done
fi
