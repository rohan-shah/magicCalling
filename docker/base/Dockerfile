FROM r-base:latest

USER root
ENV DEBIAN_FRONTEND noninteractive
COPY packages.txt .
RUN apt-get update \
 && apt-get -yq dist-upgrade \
 && cat packages.txt | xargs apt-get install -y --no-install-recommends \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY Rpackages.txt .
RUN R -e "packages <- read.table('./Rpackages.txt', stringsAsFactors=FALSE); install.packages(packages[,1]);"

RUN rm /Rpackages.txt \
 && rm /packages.txt


