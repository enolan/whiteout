#!/bin/bash
#Run this from whiteout top-level dir to build tests.

ghc --make -isrc:dist/build -outputdir dist/build -o dist/runTests -O -Wall src/Test/runTests.hs
