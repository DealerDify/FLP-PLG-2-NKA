all:
	ghc ./src/plg-2-nka.hs -o ./plg-2-nka

.PHONY: clean
clean:
	rm -f ./src/*.o ./src/*.hi ./plg-2-nka
