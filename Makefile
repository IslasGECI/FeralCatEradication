all: reports/predicting_targets_and_costs.pdf

define renderLatexBibtexAndPythontex
	cd $(@D) && pdflatex $(<F)
	cd $(<D) && pythontex $(<F)
	cd $(<D) && bibtex $(subst .tex,,$(<F))
	cd $(@D) && pdflatex $(<F)
	cd $(@D) && pdflatex $(<F)
endef

define checkDirectories
	mkdir --parents $(@D)
endef

reports/predicting_targets_and_costs.pdf: reports/predicting_targets_and_costs.tex \
	reports/figures/reduction_factor.jpg \
	reports/figures/simulation.png \
	reports/figures/constant_proportional_annual_cull.png \
	reports/figures/monthly_time_serie_individuals.jpg \
	reports/figures/culling_contour_plot.png \
	reports/figures/cost_hunt_contour_plot.png \
	reports/figures/cost_felixer_contour_plot.png \
	reports/figures/cost_traps_contour_plot.png
	$(renderLatexBibtexAndPythontex)

reports/figures/reduction_factor.jpg: src/plot_reduction_factor.R
	mkdir --parents $(@D)
	Rscript src/plot_reduction_factor.R

reports/tables/simulation.csv: src/untreated_population.R
	mkdir --parents $(@D)
	Rscript src/untreated_population.R

reports/figures/simulation.png: reports/tables/simulation.csv src/plot_untreated_population.py
	mkdir --parents $(@D)
	python src/plot_untreated_population.py \
		--input $< \
		--output $@

reports/tables/constant_proportional_annual_cull.csv: src/constant_proportional_annual_cull.R
	mkdir --parents $(@D)
	Rscript src/constant_proportional_annual_cull.R

reports/figures/constant_proportional_annual_cull.png: reports/tables/constant_proportional_annual_cull.csv src/plot_constant_proportional_annual_cull.py
	mkdir --parents $(@D)
	python src/plot_constant_proportional_annual_cull.py \
		--input $< \
		--output $@

reports/figures/monthly_time_serie_individuals.jpg: src/presentacion_210820.R
	mkdir --parents $(@D)
	Rscript src/presentacion_210820.R

reports/figures/culling_contour_plot.png: reports/tables/final_population_remaining_combinations_culling_scenarios.csv src/plot_culling_contours.py
	mkdir --parents $(@D)
	python src/plot_culling_contours.py \
		--input $< \
		--output $@

reports/tables/final_population_remaining_combinations_culling_scenarios.csv: src/cost_different_hunting_methods.R
	mkdir --parents $(@D)
	Rscript src/cost_different_hunting_methods.R

reports/figures/cost_hunt_contour_plot.png: reports/tables/final_cost_hunt_combinations_culling_scenarios.csv src/plot_cost_contours.py
	mkdir --parents $(@D)
	python src/plot_cost_contours.py \
		--input $< \
		--input Cacería \
		--output $@

reports/figures/cost_felixer_contour_plot.png: reports/tables/final_cost_felixer_combinations_culling_scenarios.csv src/plot_cost_contours.py
	mkdir --parents $(@D)
	python src/plot_cost_contours.py \
		--input $< \
		--input Felixer \
		--output $@

reports/figures/cost_traps_contour_plot.png: reports/tables/final_cost_traps_combinations_culling_scenarios.csv src/plot_cost_contours.py
	mkdir --parents $(@D)
	python src/plot_cost_contours.py \
		--input $< \
		--input Trampeo \
		--output $@

.PHONY: \
		check \
		clean \
		coverage \
		linter \
		mutants \
		results \
		setup \
		tests

define lint
	R -e "library(lintr)" \
	  -e "lint_dir('R', linters = with_defaults(line_length_linter(120)))" \
	  -e "lint_dir('tests', linters = with_defaults(line_length_linter(120)))" \
	  -e "lint_dir('src', linters = with_defaults(line_length_linter(120)))"
endef

check:
	R -e "library(styler)" \
	  -e "resumen <- style_dir('R')" \
	  -e "resumen <- rbind(resumen, style_dir('src'))" \
	  -e "resumen <- rbind(resumen, style_dir('tests'))" \
	  -e "any(resumen[[2]])" \
	  | grep FALSE

clean:
	cd reports && ls | egrep --invert-match "*.tex|*.md|*.bib" | xargs --delimiter="\n" rm --force --recursive
	rm --force --recursive popdyn.Rcheck
	rm --force --recursive reports/figures
	rm --force --recursive tests/testthat/_snaps
	rm --force popdyn_*.tar.gz
	rm --force --recursive reports/pythontex-files-predicting_targets_and_costs
	rm --force NAMESPACE
	rm --force Rplots.pdf

coverage: setup
	Rscript tests/testthat/coverage.R

format:
	R -e "library(styler)" \
	  -e "style_dir('R')" \
	  -e "style_dir('src')" \
	  -e "style_dir('tests')"

init: setup tests

linter:
	$(lint)
	if $(lint) | grep -e "\^" ; then exit 1 ; else exit 0 ; fi

mutants: tests
	@echo "🙁🏹 No mutation testing on R 👾🎉👾"

results: src/FeralCatEradication.R
	mkdir reports/figures/ --parents
	Rscript src/FeralCatEradication.R

setup: clean
	R -e "devtools::document()" && \
	R CMD build . && \
	R CMD check popdyn_0.3.0.tar.gz && \
	R CMD INSTALL popdyn_0.3.0.tar.gz
	
tests:
	R -e "devtools::test()"
