FROM rstudio/plumber
LABEL org.opencontainers.image.authors="Rolf Simoes <rolf.simoes@opengeohub.org>"

RUN apt update -y && apt install -y libudunits2-dev libgdal-dev libproj-dev
RUN R -e "install.packages('sf')"
RUN R -e "install.packages('future')"
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rolfsimoes/openstac')"

WORKDIR /openstac
COPY ./plumber /openstac
CMD ["/openstac/stac.R"]
