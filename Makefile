.PHONY: analysis comparison tables test lint check ci ci-docker

analysis:
	Rscript scripts/01_analysis.R

comparison:
	Rscript scripts/02_compare_paper.R

tables:
	Rscript scripts/03_tables.R

test:
	Rscript -e 'testthat::test_dir("tests/testthat")'

lint:
	Rscript -e 'lintr::lint_dir("R"); lintr::lint_dir("scripts"); lintr::lint_dir("tests")'

check: test lint

ci: analysis comparison tables check

ci-docker:
	docker run --rm \
		-e MAKEFLAGS="-e -j1" \
		-e CXXFLAGS="-O0 -g0" \
		-e CXX20FLAGS="-O0 -g0" \
		-e RENV_CONFIG_CACHE_ENABLED=FALSE \
		-e RENV_CONFIG_EXTERNAL_LIBRARIES=/usr/local/lib/R/site-library \
		-e RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE \
		-v "$(CURDIR):/work" -w /work rocker/tidyverse:4.6.0 \
		bash -lc 'apt-get update && apt-get install -y --no-install-recommends cmake curl git libnlopt-dev && Rscript -e '\''renv::restore(prompt = FALSE)'\'' && make ci'
