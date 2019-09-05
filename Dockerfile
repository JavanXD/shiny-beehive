# Install R version
FROM r-base:latest

LABEL version="1.0"
LABEL description="R Shiny App to visualize beehive measurement data from beehive monitoring systems."
LABEL maintainer="mail@javan.de"

# Need this to add R repo
RUN apt-get update && apt-get install -y software-properties-common

# Install basic stuff and R
RUN apt-get update && apt-get install -y \
    sudo \
    locales \
    git \
    vim-tiny \
    less \
    wget \
    r-base \
    r-base-dev \
    r-recommended \
    fonts-texgyre \
    texinfo \
    locales \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

RUN locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8

# Install stuff for Shiny server
RUN apt-get install -y \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'devtools','shinyWidgets','shinyjs'), repos='https://cran.rstudio.com/')"
# install dependencies of the app
RUN R -e "install.packages(c('ggplot2', 'readr', 'corrplot', 'dplyr', 'lubridate', 'plotly', 'forecast', 'pryr', 'ggplotify', 'ggpubr', 'DT', 'xts'), dependencies = TRUE, repos='https://packages.othr.de/cran/')"
RUN R -e "devtools::install_github('jbkunst/highcharter')"
# For deploying apps from a container
RUN R -e "devtools::install_github('rstudio/rsconnect')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
