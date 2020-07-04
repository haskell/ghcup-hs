.PHONY: clean

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: test/data/ghc-8.8.1-src.tar test/data/alsa-lib-1.1.9.tar test/data/llvm-9.0.0.src.tar test/data/ATS2-Postiats-0.3.13.tar test/data/libarchive-1.0.5.1.tar

clean:
	rm -rf dist-newstyle dist test/data/*.tar* test/data/*.tgz *.hp *.prof *.chi *.chs.h stack.yaml.lock .hspec-failures .stack-work tags *.svg

test/data:
	mkdir -p $@

test/data/ghc-8.8.1-src.tar: test/data/ghc-8.8.1-src.tar.xz
	xz -d -f $^

test/data/alsa-lib-1.1.9.tar: test/data/alsa-lib-1.1.9.tar.bz2
	bzip2 -d -f $^

test/data/llvm-9.0.0.src.tar: test/data/llvm-9.0.0.src.tar.xz
	xz -d -f $^

test/data/ATS2-Postiats-0.3.13.tar: test/data/ATS2-Postiats-0.3.13.tgz
	gunzip -f $^

test/data/libarchive-1.0.5.1.tar: test/data/libarchive-1.0.5.1.tar.gz
	gunzip -f $^

test/data/ghc-8.8.1-src.tar.xz: test/data
	wget https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-src.tar.xz -O $@

test/data/alsa-lib-1.1.9.tar.bz2: test/data
	wget https://www.alsa-project.org/files/pub/lib/alsa-lib-1.1.9.tar.bz2 -O $@

test/data/llvm-9.0.0.src.tar.xz: test/data
	wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz -O $@

test/data/ATS2-Postiats-0.3.13.tgz: test/data
	wget http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-0.3.13.tgz -O $@

test/data/libarchive-1.0.5.1.tar.gz: test/data
	wget http://hackage.haskell.org/package/libarchive-1.0.5.1/libarchive-1.0.5.1.tar.gz -O $@
