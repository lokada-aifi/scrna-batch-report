FROM us.gcr.io/dev-pipeline-internal/google-r-base:v2.0

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys KEYS-HERE

RUN apt-get update
RUN apt-get install -y python3-full
RUN apt-get install -y python3-pip 
RUN pip install plotly --break-system-packages

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
    libbz2-dev \
    libc6-dev  \
    libgcc-9-dev  \
    gcc-9-base \
    liblzma-dev \
    libgit2-dev \
    zlib1g-dev \
    pkg-config \
    libfontconfig1-dev \
    libharfbuzz-dev  \
    libfribidi-dev  \
    libfreetype-dev \
    libtiff5-dev \
    libjpeg-dev \
    git

## clean up
RUN apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

## Install R packages
RUN export R_HOME=/usr/lib/R
RUN R -e 'install.packages("devtools")'
RUN R -e 'install.packages("optparse")'
RUN R -e 'install.packages("jsonlite")'
RUN R -e 'install.packages("rmarkdown")'
RUN R -e 'install.packages("viridis")'
RUN R -e 'install.packages("assertthat")'
RUN R -e 'install.packages("rlang")'

RUN R -e 'install.packages("BiocManager");BiocManager::install()'
RUN R -e 'BiocManager::install(c("rhdf5", "GenomicRanges"))'

# Seurat package depends on this version or newer of rlang
RUN R -e 'install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.0.tar.gz", repo=NULL, type="source")'

# Newer versions of spatstat break Seurat so don't use those
RUN R -e 'remotes::install_version("spatstat", version = "1.64-1")'

# Seurat package depends on this version or newer of rlang
RUN R -e 'install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.0.tar.gz", repo=NULL, type="source")'

RUN R -e 'remotes::install_version("igraph", version = "1.3.5")'

# 'Matrix' version >= 1.5.0 is required for SeuratObject
RUN R -e 'remotes::install_version("Matrix", version = "1.6.5")'
RUN R -e 'remotes::install_version("SeuratObject", version = "4.1.3")'
RUN R -e 'remotes::install_version("Seurat", version = "4.3.0.1")'
RUN R -e 'remotes::install_version("gt", version = "0.3.1")'

# Install AIFI Github packages
COPY auth_token4 /tmp/auth_token
RUN export GITHUB_PAT=$(cat /tmp/auth_token) \
   && R -e 'devtools::install_github("aifimmunology/immutils", auth_token = Sys.getenv("GITHUB_PAT")); devtools::install_github("aifimmunology/H5weaver", auth_token = Sys.getenv("GITHUB_PAT"));   devtools::install_github("aifimmunology/HTOParser", auth_token = Sys.getenv("GITHUB_PAT")); devtools::install_github("aifimmunology/ATAComb", auth_token = Sys.getenv("GITHUB_PAT"))' \
  && git clone -b hto_addition_uk_docker_ver https://aifi-gitops:$GITHUB_PAT@github.com/aifimmunology/batchreporter.git \
  && rm -rf /tmp/downloaded_packages /tmp/*.rds /tmp/auth_token 