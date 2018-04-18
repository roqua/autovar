FROM roqua/opencpu-base

WORKDIR /autovar

RUN apt-get update && apt-get -f install -y openssl libcurl4-openssl-dev curl libxml2-dev libssl-dev libcairo-dev

ADD ./inst/bash/install-package-dependencies.sh /autovar/inst/bash/install-package-dependencies.sh

RUN /autovar/inst/bash/install-package-dependencies.sh

ADD ./ /autovar

RUN /autovar/inst/bash/timeprojection.sh
RUN R CMD INSTALL --no-multiarch --with-keep.source /autovar
RUN R CMD build /autovar
