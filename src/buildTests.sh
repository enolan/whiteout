#!/bin/bash
#Run this from whiteout top-level dir to build tests.

ghc --make -isrc:dist/build -outputdir dist/build -o dist/runTest -O -Wall src/Test/runTest.hs
