FROM opengeohub/opestac
RUN apt update -y && apt install -y libudunits2-dev libgdal-dev libproj-dev
RUN R -e "install.packages('mongolite')"
RUN R -e "install.packages('sf')"
RUN R -e "install.packages('future')" -e "install.packages('remotes')"
RUN R -e "remotes::install_github('rolfsimoes/rstac@b-1.0.0-beta')"
RUN R -e "remotes::install_github('rolfsimoes/openstac@main')"
WORKDIR /openstac
COPY ./olm /openstac
CMD ["/openstac/olm-stac.R"]
