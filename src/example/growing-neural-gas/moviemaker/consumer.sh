#!/bin/sh

# Copyright (C) 2019 Roman V. Prikhodchenko
# Author: Roman V. Prikhodchenko <chujoii@gmail.com>
# This file is part of lisp-artificial-neural-network.
# License: GPL v3 or later

EXT=$1

DIR=/tmp/ai/graphviz
IMAGES=/tmp/ai/image-cluster

FIFO=/tmp/ai/fifo-dot

MAXPROC=2

touch $FIFO

fungraphviz ()
{
    while read -r line
    do
	DOT=$line
	PC=`pgrep neato | wc -l`

	if [ $MAXPROC -gt $PC ]
	then
	    echo $DOT $PC parallel
	    neato -T$EXT -o$IMAGES/$DOT.$EXT $DIR/$DOT &
	else
	    echo $DOT $PC wait
	    neato -T$EXT -o$IMAGES/$DOT.$EXT $DIR/$DOT
	fi
    done
}

tail -f $FIFO | fungraphviz

#tail -f $FIFO | xargs -I % --max-args=1 --max-procs=$MAXPROC        neato -T$EXT -o$IMAGES/%.$EXT $DIR/%
