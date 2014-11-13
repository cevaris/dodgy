SHELL := /bin/bash
UNAME_S := $(shell uname -s)
# export PATH := "~/.cabal/bin:$$PATH"


all: test configure
	cabal build
	mkdir bin
	cp dist/build/Dodgy/Dodgy bin/Dodgy
test:

ifeq (${UNAME_S}, Linux)
ifeq ($$(which cabal),"")
    echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1;
endif
endif


configure:
ifeq (${UNAME_S}, Linux)
	echo please install the following packages "libghc-zlib-dev, libghc-zlib-bindings-dev"
endif
	
	cabal update
	cabal install cabal
	
	export PATH=~/.cabal/bin:$$PATH; cabal install --only-dependencies
	cabal configure
clean: test
	cabal clean
	- rm -fr Dodgy dist bin
	