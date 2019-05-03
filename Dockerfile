FROM r-base:3.5.1
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
    openjdk-11-jre  && \ 
    rm -rf /var/lib/apt/lists/*

# Not using because of H2O incompatibility
#default-jre && \
 
#RUN Rscript -e 'install.packages(c("glue"))'
#RUN Rscript -e 'install.packages(c("stringi"))'
#RUN Rscript -e 'install.packages(c("covr"));if (!all(c("covr") %in% installed.packages())) { q(status = 1, save = "no")}'
#RUN Rscript -e 'install.packages(c("devtools"));if (!all(c("devtools") %in% installed.packages())) { q(status = 1, save = "no")}'
#RUN Rscript -e 'install.packages(c("roxygen2"));if (!all(c("roxygen2") %in% installed.packages())) { q(status = 1, save = "no")}'

ADD ./inst/bash/install-package-dependencies.sh /app/inst/bash/install-package-dependencies.sh
ADD ./packrat /app/packrat
RUN ./inst/bash/install-package-dependencies.sh

# Install dependencies for the knitr / vignette building process
RUN R --vanilla -e "install.packages(c('knitr', 'rmarkdown'), repos='https://cran.cnr.berkeley.edu/')"

RUN mkdir -p /app/OnlineSuperLearner.Rcheck/tests/ && ln -sf /proc/self/fd/1 /app/OnlineSuperLearner.Rcheck/tests/testthat.Rout

COPY ./ /app
RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /app
RUN R CMD build /app
CMD R CMD check --no-clean /app/`ls *.gz | tail -1` --no-manual --no-build-vignettes
