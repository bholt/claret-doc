#!/bin/bash
for f in out/figure/*.pdf; do
  pdf2svg $f ${f/.pdf/.svg}
done
