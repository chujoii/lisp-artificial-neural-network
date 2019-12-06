#!/bin/sh

# Copyright (C) 2019 Roman V. Prikhodchenko
# Author: Roman V. Prikhodchenko <chujoii@gmail.com>
# This file is part of lisp-artificial-neural-network.
# License: GPL v3 or later

DIR=/tmp/ai/graphviz
IMAGES=/tmp/ai/image-cluster

mkdir -p $DIR
mkdir -p $IMAGES

while RES=$(inotifywait -e create $DIR --format %f .)
do
    echo RES is $RES at `date`

    # very danger: in slow system many parallel "neato" process run
    # and not hold terminal, because "&"
    # solution: queue using fifo
    neato -Tpng -o$IMAGES/$RES.png $DIR/$RES &
done
