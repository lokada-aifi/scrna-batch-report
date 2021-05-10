FROM us.gcr.io/dev-pipeline-internal/google-r-base:v2.0

## Install apt dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    apt-utils \
    gfortran \
    liblapack-dev \
    liblapack3 \
    libopenblas-base \
    libopenblas-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    pandoc \
    libhdf5-dev \
    git

## clean up
RUN apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


## Install R packages
RUN R -e 'install.packages(c("devtools"))'

## CRAN dependencies
RUN R -e 'install.packages(c("jsonlite","rmarkdown","optparse"))'


## Install AIFI Github packages
COPY auth_token /tmp/auth_token
RUN export GITHUB_PAT=$(cat /tmp/auth_token) \
   && R -e   'devtools::install_github("aifimmunology/immutils", auth_token = Sys.getenv("GITHUB_PAT")); devtools::install_github("aifimmunology/H5weaver", auth_token = Sys.getenv("GITHUB_PAT"));   devtools::install_github("aifimmunology/HTOParser", auth_token = Sys.getenv("GITHUB_PAT")); devtools::install_github("aifimmunology/batchreporter", auth_token = Sys.getenv("GITHUB_PAT"))' \
  && git  clone  https://aifi-gitops:$GITHUB_PAT@github.com/aifimmunology/batchreporter.git \
  && rm -rf /tmp/downloaded_packages /tmp/*.rds /tmp/auth_token

## AIFI Pipeline package requirements
RUN R -e 'install.packages(c("rmarkdown","optparse"))'


