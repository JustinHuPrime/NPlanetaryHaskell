.PHONY: all server client clean

all: server client

server:
	ghc ServerMain.hs -o server

client:
	ghc ClientMain.hs -o client

clean:
	rm -f server client
	find . -name "*.o" -delete
	find . -name "*.hi" -delete