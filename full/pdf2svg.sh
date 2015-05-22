#!/bin/bash
if [ -d out/figure ]; then
  for f in out/figure/*.pdf; do
    pdf2svg $f ${f/.pdf/.svg}
  done
fi
