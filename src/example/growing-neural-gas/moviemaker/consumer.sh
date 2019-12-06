#!/bin/sh

# Copyright (C) 2019 Roman V. Prikhodchenko
# Author: Roman V. Prikhodchenko <chujoii@gmail.com>
# This file is part of lisp-artificial-neural-network.
# License: GPL v3 or later

DIR=/tmp/ai/graphviz
IMAGES=/tmp/ai/image-cluster

FIFO=/tmp/ai/fifo-dot

while [[ true ]]; do
    DOT=`cat $FIFO`
    echo $DOT
    neato -Tpng -o$IMAGES/$DOT.png $DIR/$DOT
done
