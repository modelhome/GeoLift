# Build:
#   docker build -t geolift-runner:local .
#
# Run:
#   docker run --rm geolift-runner:local

FROM rocker/r-ver:4.3.3

WORKDIR /app

ENV CRAN_REPO=https://packagemanager.posit.co/cran/latest

COPY DESCRIPTION NAMESPACE ./

RUN R -e "options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); install.packages(c('remotes', 'jsonlite'))" \
    && R -e "options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); remotes::install_deps('.', dependencies = TRUE, upgrade = 'never')" \
    && R -e "options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); remotes::install_github('ebenmichael/augsynth')" \
    && true

COPY R ./R
COPY LICENSE.md README.md ./

RUN R -e "options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); remotes::install_local('.', upgrade = 'never', dependencies = FALSE)"

COPY runner.R demo_input.json ./

ENTRYPOINT ["Rscript", "runner.R"]
CMD ["demo_input.json"]
