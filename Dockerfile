FROM rocker/r-base

RUN R -e "install.packages('renv')"

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libv8-dev \
    libsodium-dev

COPY . /usr/local/Chronos-Scheduler/

WORKDIR /usr/local/Chronos-Scheduler/

RUN R -e "source('.Rprofile')"

RUN R -e "renv::restore()"

CMD ["R", "-e", "source('/usr/local/Chronos-Scheduler/crontab.R')"]
