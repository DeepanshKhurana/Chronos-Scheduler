FROM rocker/r-ver:4.5

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev libssl-dev libxml2-dev libpq-dev \
    libv8-dev libsodium-dev libgit2-dev git tzdata ca-certificates \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/local/Chronos-Scheduler
COPY . .

RUN R -e "renv::restore()"

CMD ["R","-e","source('/usr/local/Chronos-Scheduler/crontab.R')"]
