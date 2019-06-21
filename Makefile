.PHONY: all build check document test

all: document build check

build: document
	R CMD build .

check: build
	R CMD check injectr*tar.gz

clean:
	-rm -f injectr*tar.gz
	-rm -fr injectr.Rcheck

document:
	Rscript -e 'devtools::document()'

test:
	Rscript -e 'devtools::test()'
