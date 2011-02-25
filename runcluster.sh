#!/usr/bin/env bash
numpart=`wc -l $1 | cut -d ' ' -f 1`
echo "número de particulas es: $numpart"
echo "particulas: $1"
echo "configuración: $2"
./cluster $numpart $1 $2 | awk '{ if ($3 != -1) { print }}' | sort -n -k3 