# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install libxml2-dev libcairo2-dev libsqlite3-dev libmariadbd-dev libpq-dev libssh2-1-dev unixodbc-dev libcurl4-openssl-dev libssl-dev

## update system libraries
RUN apt-get update && apt-get upgrade -y && apt-get clean

## install renv
ENV RENV_VERSION 0.17.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

## app folder
COPY /app ./app
## renv.lock file
COPY /app/renv.lock ./renv.lock

##  restore packages
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]