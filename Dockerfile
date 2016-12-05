# How to test using OpenCPU:
#   1. docker build -t autovar .
#   2. docker run -p 80:80 autovar
#   3. Go to <docker IP>/ocpu/test

FROM roqua/opencpu-base
ADD . /autovar
WORKDIR /autovar


ADD ./docker_configs/opencpu_server.conf.patch /docker_configs/opencpu_server.conf.patch
RUN patch -p0 -d /etc/opencpu < /docker_configs/opencpu_server.conf.patch

RUN Rscript inst/bash/install-package-dependencies.sh
RUN R -e 'library("devtools"); install.packages(build(".", path = "."));'
