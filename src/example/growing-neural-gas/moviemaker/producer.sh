#!/bin/sh

# Copyright (C) 2019 Roman V. Prikhodchenko
# Author: Roman V. Prikhodchenko <chujoii@gmail.com>
# This file is part of lisp-artificial-neural-network.
# License: GPL v3 or later


DIR=/tmp/ai/graphviz
IMAGES=/tmp/ai/image-cluster
FIFO=/tmp/ai/fifo-dot

mkdir -p $DIR
mkdir -p $IMAGES

touch $FIFO

while RES=$(inotifywait -e create $DIR --format %f .)
do
    echo RES is $RES at `date`
    echo $RES >> $FIFO
done
