# Makefile for for decisions

DSize=240
TRatio=0.7

install:
	cabal --force-reinstalls install

run:
	cabal run prep $(DSize) $(TRatio) && cabal run train && cabal run test

server:
	python -m SimpleHTTPServer 8000 && open "http://localhost:8000"
