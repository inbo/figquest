FROM rocker/verse

ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.name="checklist" \
      org.label-schema.description="A docker image dedicated to thoroughly checking R packages and code." \
      org.label-schema.license="MIT" \
      org.label-schema.url="e.g. https://www.inbo.be/" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.vcs-url="https://github.com/inbo/checklist" \
      org.label-schema.vendor="Research Institute for Nature and Forest" \
      maintainer="Thierry Onkelinx <thierry.onkelinx@inbo.be>"

## for apt to be noninteractive
ENV DEBIAN_FRONTEND noninteractive
ENV DEBCONF_NONINTERACTIVE_SEEN true

## Install nano
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    nano

COPY docker/.Rprofile $R_HOME/etc/Rprofile.site

# install Imports dependencies
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("assertthat")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("dplyr")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("effectclass")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("ggplot2")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("git2rdata")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("INBOtheme")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("purrr")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("rlang")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("scales")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("stringr")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("tidyr")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("tidyselect")'

# install Suggests dependencies
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("gistr")'
RUN Rscript --no-save --no-restore -e 'remotes::install_cran("shiny")'

# install figquest
COPY R /tmp/figquest/R
COPY man /tmp/figquest/man
COPY DESCRIPTION /tmp/figquest/DESCRIPTION
COPY NAMESPACE /tmp/figquest/NAMESPACE
COPY inst/base_data.tsv /tmp/figquest/inst/base_data.tsv
COPY inst/base_data.yml /tmp/figquest/inst/base_data.yml
COPY inst/figquest /tmp/figquest/inst/figquest
RUN Rscript -e 'remotes::install_local("/tmp/figquest", dependencies = FALSE)'

EXPOSE 3838

CMD ["R", "-e figquest::run_app()"]
