FROM roqua/opencpu-base

WORKDIR /autovar

RUN apt-get update && apt-get -f install -y openssl libcurl4-openssl-dev curl libxml2-dev libssl-dev libcairo-dev

ADD ./inst/bash/install-package-dependencies.sh /autovar/inst/bash/install-package-dependencies.sh

RUN ./inst/bash/install-package-dependencies.sh

ADD ./ /autovar

RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /autovar
RUN R CMD build /autovar

