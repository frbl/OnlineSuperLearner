FROM r-base

WORKDIR /OnlineSuperLearner

RUN apt-get update && apt-get -f install -y openssl libcurl4-openssl-dev curl libxml2-dev libssl-dev libcairo-dev libnlopt0 default-jre
 
ADD ./inst/bash/install-package-dependencies.sh /OnlineSuperLearner/inst/bash/install-package-dependencies.sh

RUN ./inst/bash/install-package-dependencies.sh

ADD ./ /OnlineSuperLearner

RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /OnlineSuperLearner
RUN R CMD build /OnlineSuperLearner

CMD R CMD check /OnlineSuperLearner/`ls *.gz | tail -1` --no-manual --no-build-vignettes
