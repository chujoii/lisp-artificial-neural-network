#!/bin/sh

function fungnuplot() { gnuplot -e  "set terminal png; set output '$1.png'; plot '$1'"; }

for i in *
do
    echo $i
    $* $i
done
