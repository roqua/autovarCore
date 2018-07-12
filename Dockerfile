# How to test using OpenCPU:
#   1. docker build -t autovarCore .
#   2. docker run -p 80:80 autovarCore
#   3. Go to <docker IP>/ocpu/test

FROM compsy/opencpu-base

WORKDIR /autovarCore

RUN apt-get update
RUN apt-get install -y git-core pandoc

ADD ./inst/bash/install-package-dependencies.sh /autovarCore/inst/bash/install-package-dependencies.sh
RUN /autovarCore/inst/bash/install-package-dependencies.sh

ADD ./docker_configs/opencpu_server.conf.patch /docker_configs/opencpu_server.conf.patch
RUN patch -p0 -d /etc/opencpu < /docker_configs/opencpu_server.conf.patch

ADD ./ /autovarCore

#RUN R -e 'library("devtools"); install.packages(build(".", path = "."));'
RUN R CMD INSTALL --no-multiarch --with-keep.source /autovarCore
RUN R CMD build /autovarCore
