##################################################################################################################################################################
# Adapted from following:
# - Rocker RStudio container using new versioned paradigm: https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/Dockerfile_rstudio_4.0.2
# - license: GPLV2
##################################################################################################################################################################

FROM rocker/r-ver:4.1.0

ENV S6_VERSION=v1.21.7.0
ENV RSTUDIO_VERSION=preview
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

# install key dependencies of rstudio preview edition
# install radian via python and pip3
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends libpq5 openssh-client openssh-server

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

# install key dependencies of certain packages that could be installed later
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends software-properties-common libssl-dev libxml2-dev libcurl4-openssl-dev python3-pip pandoc \
    && apt-get -y install --no-install-recommends libfreetype6-dev libfontconfig1-dev tk libgit2-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* 

# install R packages needed for VSCode interaction and package management 
RUN install2.r languageserver renv remotes reticulate golem crul curl httr xml2 cachem shinyWidgets dplyr purrr ggplot2 tidyr fs glue calendar devtools rmarkdown clock
RUN installGithub.r dreamRs/toastui ColinFay/brochure ColinFay/glouton petermeissner/caldav koderkow/twitchr

# install radian via python and pip3
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends python3-setuptools python3-pip libpython3-dev

RUN pip3 install radian caldav icalendar numpy

RUN echo "RENV_PATHS_CACHE=/renv/cache" >> /usr/local/lib/R/etc/Renviron

# [Optional] Uncomment this section to add addtional system dependencies needed for project
# RUN apt-get update \
#     && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends ---packages list----

EXPOSE 8787

CMD ["/init"]
