v1:
	cabal build
	cp $(shell cabal list-bin exe:speedtests) ./main

strip:
	strip -s main

clean:
	rm main main.o main.hi
