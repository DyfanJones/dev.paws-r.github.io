# Make R use the user's package library by setting the R user home path (R_USER)
# to the folder containing their package library. On Windows, it is in
# ~/Documents/R, whereas in Linux/macOS it is in ~/R.
ifdef OS
	R_USER := ${HOME}
else
	R_USER := ${HOME}
endif
export R_USER

.PHONY: all update-deps clean-down build-docs build-site regen-site install-package test-package requirements

update-deps:
	@echo "update paws dependency"
	@git submodule init
	@git submodule update --remote

clean-down:
	@echo "INFO $$(date +%F) $$(date +%T): Clearing down site"
	@rm -rf docs
	@rm -rf build/mkdocs/site

build-docs: clean-down
	@Rscript -e "library(paws.site.builder); build_rd_to_md(); build_all_assets()"

build-site: build-docs
	@echo "INFO $$(date +%F) $$(date +%T): Building site"
	@cd build/mkdocs && python -m mkdocs build

regen-site: build-site
	@echo "INFO $$(date +%F) $$(date +%T): Moving site to root"
	@rm -rf build/mkdocs/docs

install-package:
	@echo "INFO $$(date +%F) $$(date +%T): Installing paws.site.builder package"
	@Rscript -e "install.packages('r-pkg', repos = NULL, type = 'source')"

test-package:
	@echo "INFO $$(date +%F) $$(date +%T): Running paws.site.builder R CMD check"
	@Rscript -e "rcmdcheck::rcmdcheck('r-pkg', args = c('--no-manual', '--as-cran'), error_on = 'warning', check_dir = 'check')"

requirements:
	@echo "INFO $$(date +%F) $$(date +%T): Installing R dependencies"
	@Rscript -e "install.packages(c('rmarkdown', 'fs', 'yaml', 'roxygen2', 'remotes', 'mirai', 'heck', 'testthat', 'withr', 'rcmdcheck'), repos='https://cran.rstudio.com/')"
	@echo "INFO $$(date +%F) $$(date +%T): Installing paws.site.builder package"
	@Rscript -e "install.packages('r-pkg', repos = NULL, type = 'source')"
	@echo "INFO $$(date +%F) $$(date +%T): Installing Python dependencies"
	@python -m pip install --upgrade pip
	@pip install --upgrade mkdocs-material awscli
