#!/bin/bash
for f in figure/*.pdf; do
  pdf2svg $f ${f/.pdf/.svg}
done
