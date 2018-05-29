FROM r-base:3.4.3
MAINTAINER Frank Blaauw <f.j.blaauw@rug.nl>

WORKDIR /OnlineSuperLearner
COPY ./inst/bash/install-package-dependencies.sh /OnlineSuperLearner/inst/bash/install-package-dependencies.sh

# The unstable flag was needed to install the correct curl libraries.
# See https://github.com/rocker-org/rocker/issues/232.
RUN apt-get update && apt-get -f install -t unstable --no-install-recommends -y \
    libnlopt0 \
    openssl \
    libcurl4-openssl-dev \
    curl \
    git \
    libxml2-dev \
    libssl-dev \
    libcairo-dev \ 
    default-jre && \
    rm -rf /var/lib/apt/lists/*
 
RUN ./inst/bash/install-package-dependencies.sh

#RUN Rscript -e 'install.packages(c("covr"));if (!all(c("covr") %in% installed.packages())) { q(status = 1, save = "no")}'
#RUN Rscript -e 'install.packages(c("devtools"));if (!all(c("devtools") %in% installed.packages())) { q(status = 1, save = "no")}'
#RUN Rscript -e 'install.packages(c("roxygen2"));if (!all(c("roxygen2") %in% installed.packages())) { q(status = 1, save = "no")}'

COPY ./ /OnlineSuperLearner
#RUN Rscript -e 'devtools::install_deps("/OnlineSuperLearner")'

RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /OnlineSuperLearner
RUN R CMD build /OnlineSuperLearner
CMD R CMD check /OnlineSuperLearner/`ls *.gz | tail -1` --no-manual --no-build-vignettes
