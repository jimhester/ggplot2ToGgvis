R_SOURCE = $(wildcard R/*.r)

all: ggplot2ToGgvis.html

%.html: %.Rmd $(R_SOURCE)
	PATH=$$HOME/.cabal/bin:$$PATH Rscript -e "\
    library(knitr);\
    library(knitrBootstrap);\
    library(rmarkdown);\
    render('$^')"

publish: ggplot2ToGgvis.html
	git checkout gh-pages
	cp plyrToDplyr.html index.html

clean:
	rm -rf *.html *_{files,cache} R/*.md
