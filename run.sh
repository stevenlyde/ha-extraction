#!/bin/sh

cat $1 | racket src/desugar.rkt | racket src/anf.rkt | racket src/cfa.rkt | racket src/simpha.rkt

