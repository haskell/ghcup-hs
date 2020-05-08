HSC=ghc
CC=gcc # can also use $(HSC)

UNCOMPRESSED=`find .. -type f`
GZIPPED=`find .. -type f -name \*.gz`

HSC_OPTS=-hide-package zlib -i

HSBINS=hszpipe hsgzip hsgunzip test-hs
BINS=$(HSBINS) zpipe

ALL:

clean:
	rm -f $(BINS) *.o *.hi

bins: hszpipe zpipe hsgzip hsgunzip

%.hs : %.hsc
	hsc2hs --cc=$(HSC) --ld=$(HSC) --lflag=-lz -o $@ $<

hs%: ../examples/%.hs
	$(HSC) $(HSC_OPTS) -lz -i.. -o $@ --make $<

$(HSBINS) : ../Codec/Compression/Zlib/Stream.hs

test-hs: Test.hs
	$(HSC) $(HSC_OPTS) -Wall -lz -i. -i.. -o $@ --make $<

zpipe: zpipe.c
	$(CC) -lz -o zpipe zpipe.c

test: run-test-hs run-test-sh-zpipe run-test-sh-gzip run-test-sh-gunzip

run-test-hs: test-hs
	./test-hs

run-test-sh-zpipe: zpipe hszpipe
	./zpipe-compress.sh $(UNCOMPRESSED)

run-test-sh-gzip: hsgzip hsgunzip
	./gzip-compress.sh $(UNCOMPRESSED)

run-test-sh-gunzip: hsgunzip
	./gzip-uncompress.sh $(GZIPPED)
