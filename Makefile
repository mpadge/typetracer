.PHONY: all build check document test

all: document build check

build: document
	R CMD build .

check: build
	R CMD check injectr*tar.gz

clean:
	-rm -f injectr*tar.gz
	-rm -fr injectr.Rcheck
	-rm -fr src/*.{o,so}

document: clean
	Rscript -e 'devtools::document()'
	Rscript -e 'rmarkdown::render("README.Rmd")'

test:
	Rscript -e 'devtools::test()'

install: clean
	R CMD INSTALL .
