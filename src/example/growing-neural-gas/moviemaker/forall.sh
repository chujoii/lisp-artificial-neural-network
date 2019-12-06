#!/bin/sh

# Copyright (C) 2019 Roman V. Prikhodchenko
# Author: Roman V. Prikhodchenko <chujoii@gmail.com>
# This file is part of lisp-artificial-neural-network.
# License: GPL v3 or later

function fungnuplot() { gnuplot -e  "set terminal png; set output '/tmp/ai/image-2D/$1.png'; plot '$1'"; }

for i in *
do
    echo $i
    $* $i
done
