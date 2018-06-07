# How to test using OpenCPU:
#   1. docker build -t autovar .
#   2. docker run -p 80:80 autovar
#   3. Go to <docker IP>/ocpu/test

# Use builds from launchpad
FROM compsy/opencpu-base

WORKDIR /autovar

ADD ./inst/bash/install-package-dependencies.sh /autovar/inst/bash/install-package-dependencies.sh
RUN /autovar/inst/bash/install-package-dependencies.sh

ADD ./docker_configs/opencpu_server.conf.patch /docker_configs/opencpu_server.conf.patch
RUN patch -p0 -d /etc/opencpu < /docker_configs/opencpu_server.conf.patch

ADD ./ /autovar

RUN /autovar/inst/bash/timeprojection.sh
#RUN R -e 'library("devtools"); install.packages(build(".", path = "."));'
RUN R CMD INSTALL --no-multiarch --with-keep.source /autovar
RUN R CMD build /autovar
