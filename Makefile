all:
	idris --build network.ipkg
	idris --install network.ipkg

build:
	idris --build network.ipkg

install:
	idris --install network.ipkg

clean:
	idris --clean network.ipkg
