########################################################
# Everything needed for generating Claret plots with R.
# 
# The included `Rscript` script runs Rscript inside the 
# Docker environment.
# 
# Usage:
#     $ docker/Rscript <plot.r>
# 
########################################################
FROM rocker/r-base
MAINTAINER Brandon Holt <holt.bg@gmail.com>

RUN apt-get update && \
    apt-get install -y \
      make \
      texlive-fonts-recommended \
      texlive-humanities \
      texlive-latex-extra \
      texinfo \
      zsh \
      mysql-client && \
    apt-get install -y -t unstable \
      libcairo2-dev \
      libmysqlclient-dev \
      libsqlite3-dev \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev

RUN install2.r --error \
  ggplot2 \
  devtools \
  plyr \
  dplyr \
  reshape2 \
  sitools \
  jsonlite \
  yaml \
  sqldf \
  RMySQL \
  scales \
  Unicode \
  extrafont \
  Hmisc
  
RUN install2.r --error TTR
    
ENV USER root

ADD dots/Rprofile /root/.Rprofile
ADD dots/my.cnf /root/.my.cnf

VOLUME /src
WORKDIR /src

CMD ["R"]
