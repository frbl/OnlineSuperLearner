FROM r-base:3.4.3
MAINTAINER Frank Blaauw <f.j.blaauw@rug.nl>

WORKDIR /app

# The unstable flag was needed to install the correct curl libraries.
# See https://github.com/rocker-org/rocker/issues/232.
#RUN apt-get update && apt-get -f install -t unstable --no-install-recommends -y \
RUN apt-get update && apt-get install --no-install-recommends -y \
    libnlopt0 \
    openssl \
    libcurl4-openssl-dev \
    curl \
    git \
    libxml2-dev \
    libssl-dev \
    libcairo-dev \ 
    openjdk-8-jre && \ 
    rm -rf /var/lib/apt/lists/*

# Not using because of H2O incompatibility
#default-jre && \
 
RUN Rscript -e 'install.packages(c("glue"))'
RUN Rscript -e 'install.packages(c("stringi"))'
RUN Rscript -e 'install.packages(c("covr"));if (!all(c("covr") %in% installed.packages())) { q(status = 1, save = "no")}'
RUN Rscript -e 'install.packages(c("devtools"));if (!all(c("devtools") %in% installed.packages())) { q(status = 1, save = "no")}'
RUN Rscript -e 'install.packages(c("roxygen2"));if (!all(c("roxygen2") %in% installed.packages())) { q(status = 1, save = "no")}'

COPY ./inst/bash/install-package-dependencies.sh /app/inst/bash/install-package-dependencies.sh
RUN ./inst/bash/install-package-dependencies.sh
RUN mkdir -p /app/OnlineSuperLearner.Rcheck/tests/ && ln -sf /proc/self/fd/1 /app/OnlineSuperLearner.Rcheck/tests/testthat.Rout


COPY ./ /app
#RUN Rscript -e 'devtools::install_deps("/OnlineSuperLearner")'

RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /app
RUN R CMD build /app
CMD R CMD check /app/`ls *.gz | tail -1` --no-manual --no-build-vignettes
