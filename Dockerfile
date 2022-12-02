FROM islasgeci/base:1.0.0
COPY . /workdir
RUN Rscript -e "install.packages(c('comprehenr', 'latex2exp', 'plotly'), repos='http://cran.rstudio.com')"

RUN R CMD build . && \
	R CMD INSTALL FeralCatEradication_0.3.0.tar.gz
