#!/bin/bash

# Comprueba que un fichero 'output-N.dat' contenga una lista de P primos
# menores que M, coincidente con los primeros P primos.

FILE=$1
MAX=$2
OUT=${FILE}.tmp
WD=$(dirname "$0")

# Poner la salida en primo por línea
sed -e 's/^\[//g' -e 's/\]$/\n/g' -e 's/,/\n/g' $FILE > $OUT

awk '$1 < '$MAX' { print $1 }' "$WD/primos-10000000.txt" | diff $OUT - > /dev/null

EQUAL=$?

rm $OUT

echo $EQUAL
