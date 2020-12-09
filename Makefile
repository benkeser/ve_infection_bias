.PHONY: md
md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"