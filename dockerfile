
#################
## Compile CPT ##
#################

# Create image
FROM debian:bullseye-slim AS cpt_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Define CPT version to be used (https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact)
ARG CPT_VERSION="15.7.11"

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # GCC5.x
        gcc make git \
        # GFortran
        gfortran \
        # wget
        ca-certificates wget \
        # to install ncdf4
        libnetcdf-dev && \
    rm -rf /var/lib/apt/lists/*

# Create CPT directory
RUN mkdir /opt/CPT

# Download CPT
# version 15.6.3
#   web: https://academiccommons.columbia.edu/doi/10.7916/D8DJ6NDS
#   download link: https://academiccommons.columbia.edu/doi/10.7916/D88S5WQH/download
RUN wget --directory-prefix=/tmp/ --content-disposition \
    https://academiccommons.columbia.edu/doi/10.7916/D88S5WQH/download
# version 15.7.11
#   web: https://academiccommons.columbia.edu/doi/10.7916/d8-kb0s-2816
#   download link: https://academiccommons.columbia.edu/doi/10.7916/d8-enx1-j159/download
RUN wget --directory-prefix=/tmp/ --content-disposition \
    https://academiccommons.columbia.edu/doi/10.7916/d8-enx1-j159/download
# version 16.5.8
#   web: https://academiccommons.columbia.edu/doi/10.7916/d8-em5q-0f07
#   download link: https://academiccommons.columbia.edu/doi/10.7916/d8-6r1c-6146/download
RUN wget --directory-prefix=/tmp/ --content-disposition \
    https://academiccommons.columbia.edu/doi/10.7916/d8-6r1c-6146/download
# version 17.3.1
#   web: https://academiccommons.columbia.edu/doi/10.7916/d8-eepd-fm34
#   downloas link: https://academiccommons.columbia.edu/doi/10.7916/d8-ax2z-d749/download
RUN wget --directory-prefix=/tmp/ --content-disposition \
    https://academiccommons.columbia.edu/doi/10.7916/d8-ax2z-d749/download

# Extraer CPT.15.6.3.tar.gz
RUN tar -xzf /tmp/CPT.15.6.3.tar.gz -C /tmp
# Extraer CPT.15.7.11.tar.gz
RUN tar -xzf /tmp/CPT.15.7.11.tar.gz -C /tmp
# Extraer CPT.16.5.8.tar.gz
RUN tar -xzf /tmp/CPT.16.5.8.tar.gz -C /tmp
# Extraer CPT.17.3.1.tar.gz
RUN tar -xzf /tmp/CPT.17.3.1.tar.gz -C /tmp

# Mover código versión 15.6.3 a src
RUN mkdir /opt/CPT/15.6.3
RUN mkdir /opt/CPT/15.6.3/src
RUN mv /tmp/CPT/15.6.3/* /opt/CPT/15.6.3/src/
# Mover código versión 15.7.11 a src
RUN mkdir /opt/CPT/15.7.11
RUN mkdir /opt/CPT/15.7.11/src
RUN mv /tmp/CPT/15.7.11/* /opt/CPT/15.7.11/src/
# Mover código versión 16.5.8 a src
RUN mkdir /opt/CPT/16.5.8
RUN mkdir /opt/CPT/16.5.8/src
RUN mv /tmp/CPT/16.5.8/* /opt/CPT/16.5.8/src/
# Mover código versión 17.3.1 a src
RUN mkdir /opt/CPT/17.3.1
RUN mkdir /opt/CPT/17.3.1/src
RUN mv /tmp/CPT/17.3.1/* /opt/CPT/17.3.1/src/

# Instalar versión 15.6.3
# Acceder al código fuente
WORKDIR /opt/CPT/15.6.3/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=/opt/CPT/15.6.3/ install

# Instalar versión 15.7.11
# Acceder al código fuente
WORKDIR /opt/CPT/15.7.11/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=/opt/CPT/15.7.11/ install

# Instalar versión 16.5.8
# Acceder al código fuente
WORKDIR /opt/CPT/16.5.8/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=/opt/CPT/16.5.8/ install

# Instalar versión 17.3.1
# Acceder al código fuente
WORKDIR /opt/CPT/17.3.1/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=/opt/CPT/17.3.1/ install

# Configurar version por defecto para CPT
RUN echo "export CPT_BIN_DIR=/opt/CPT/$CPT_VERSION/bin" >> /root/.bashrc
RUN echo "export PATH=/opt/CPT/$CPT_VERSION/bin:$PATH" >> /root/.bashrc



#############################
## Install python packages ##
#############################

# Create image
FROM python:3.9-slim-bullseye AS py_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# set python environment variables
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

# install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # to install cartopy
        proj-bin libproj-dev libgeos-dev \
        # to install rpy2
        r-base && \
    rm -rf /var/lib/apt/lists/*

# set work directory
WORKDIR /usr/src/app

# upgrade pip and install dependencies
COPY requirements.txt /tmp/requirements.txt
RUN python3 -m pip install --upgrade pip && \
    # in order to install cartopy, see: https://github.com/SciTools/cartopy/issues/1552
    python3 -m pip install numpy && \
    # finally, install python dependencies
    python3 -m pip wheel --no-cache-dir --no-deps \
    --wheel-dir /usr/src/app/wheels -r /tmp/requirements.txt



########################
## Install R packages ##
########################

# Create image
FROM python:3.9-slim-bullseye AS r_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # install R
        r-base \
        # to install ncdf4
        libnetcdf-dev \
        # to install terra, a dependency of raster
        libgdal-dev \
        # to install classInt, a dependency of sf
        gfortran \
        # to install units, a dependency of sf
        libudunits2-dev \
        # to install systemfonts, a dependency of ggiraph
        libfontconfig1-dev && \
    rm -rf /var/lib/apt/lists/*

# install R packages
RUN R -e "options(warn=2); install.packages('sp', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('ncdf4', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('raster', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('dplyr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tibble', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('sf', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('stringr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tidyr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('purrr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('yaml', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('glue', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('lubridate', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('gstat', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('rasterVis', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('htmltools', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('plainview', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leafem', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('ggplot2', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('rnaturalearth', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('RColorBrewer', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('httr', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('jsonlite', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('lattice', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('leaflet.extras2', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('RCurl', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('rnaturalearthdata', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('ggiraph', repos='https://cran.r-project.org/', verbose=T, quiet=T, keep_outputs='/tmp/')"



########################
## CREATE PYCPT IMAGE ##
########################

# Create image
FROM python:3.9-slim-bullseye AS pycpt

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Define CPT version to be used (https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact)
ARG CPT_VERSION="15.7.11"

# Install OS packages
RUN apt-get -y -qq update &&\
    apt-get -y -qq --no-install-recommends install \
        # install R
        r-base \
        # to be able to use cartopy (Python)
        proj-bin libproj-dev libgeos-dev \
        # to be able to use rpy2 (R-Python)
        libblas-dev \
        # to be able to use ncdf4 (R)
        libnetcdf-dev \
        # to install terra, a dependency of raster (R)
        libgdal-dev \
        # to be able to import and use units (R)
        libudunits2-dev \
        # to be able to use htmlwidgets (R)
        pandoc \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop \
        # to run sudo
        sudo \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Setup cron to allow it run as a non root user
RUN sudo chmod u+s $(which cron)

# Copy CPT executable from cpt_builder
COPY --from=cpt_builder /opt/CPT/$CPT_VERSION /opt/CPT

# Setup CPT for root
RUN echo "export CPT_BIN_DIR=/opt/CPT/bin" >> /root/.bashrc
RUN echo "export PATH=/opt/CPT/bin:$PATH" >> /root/.bashrc

# Install python dependencies from py_builder
COPY --from=py_builder /usr/src/app/wheels /wheels
RUN python3 -m pip install --upgrade pip && \
    python3 -m pip install --no-cache /wheels/* && \
    rm -rf /wheels

# Install R packages from r_builder
# https://forums.docker.com/t/using-multi-stage-docker-build-for-slimming-down-images-with-r-dependency/67967
RUN mkdir -p /usr/lib/R \
             /usr/local/lib/R/site-library
COPY --from=r_builder /usr/bin/R /usr/bin/R
COPY --from=r_builder /usr/bin/Rscript /usr/bin/Rscript
COPY --from=r_builder /usr/lib/R /usr/lib/R
COPY --from=r_builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=r_builder /tmp /tmp

# Create work directory
RUN mkdir -p /opt/pyCPT

# Set work directory
WORKDIR /opt/pyCPT

# Copy app code
COPY . .

# Create input and output folder (these folders are too big so they must be used them as volumes)
RUN mkdir -p /opt/pyCPT/input
RUN mkdir -p /opt/pyCPT/output
RUN mkdir -p /opt/pyCPT/plots



########################
## SETUP PYCPT IMAGE ##
########################

# Create image
FROM pycpt

# Set passwords
ARG ROOT_PWD="cpt"
ARG USER_PWD="cpt"

# Pasar a root
USER root

# Modify root password
RUN echo "root:$ROOT_PWD" | chpasswd

# Create a new user, so the container can run as non-root
# OBS: the UID and GID must be the same as the user that own the
# input and the output volumes, so there isn't perms problems!!
ARG CPT_USER="cpt"
ARG USER_UID="1000"
ARG USER_GID="1000"
RUN groupadd --gid $USER_GID $CPT_USER
RUN useradd --uid $USER_UID --gid $USER_GID --comment "CPT User Account" --create-home $CPT_USER

# Modify the password of the new user
RUN echo "$CPT_USER:$USER_PWD" | chpasswd

# Add new user to sudoers
RUN adduser $CPT_USER sudo

# Setup CPT
RUN echo "export CPT_BIN_DIR=/opt/CPT/bin" >> /home/$CPT_USER/.bashrc
RUN echo "export PATH=/opt/CPT/bin:$PATH" >> /home/$CPT_USER/.bashrc
RUN chown -R $USER_UID:$USER_UID /opt/CPT

# Setup pyCPT
RUN chown -R $USER_UID:$USER_UID /opt/pyCPT

# Setup cron for run once a month
RUN (echo "0 0 15 * * /usr/local/bin/python /opt/pyCPT/main.py >> /proc/1/fd/1 2>> /proc/1/fd/1") | crontab -u $CPT_USER -
RUN (crontab -u $CPT_USER -l; echo "0 0 16 * * /usr/bin/Rscript /opt/pyCPT/plot.R >> /proc/1/fd/1 2>> /proc/1/fd/1") | crontab -u $CPT_USER -

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD ["cron", "-f"]
# or docker run your-image /your/program ...

# Access user directory
WORKDIR /home/$CPT_USER

# Switch back to cpt_user to avoid accidental container runs as root
USER $CPT_USER

# CONSTRUIR CONTENEDOR
# docker build -f dockerfile \
#        --build-arg ROOT_PWD=cpt \
#        --build-arg USER_PWD=cpt \
#        --build-arg USER_UID=$(stat -c "%u" .) \
#        --build-arg USER_GID=$(stat -c "%g" .) \
#        -t cpt .

# CORRER OPERACIONALMENTE CON CRON
# docker run --name pycpt --rm \
#        --volume $(pwd)/input:/opt/pyCPT/input \
#        --volume $(pwd)/output:/opt/pyCPT/output \
#        --volume $(pwd)/plots:/opt/pyCPT/plots \
#        --volume $(pwd)/config.yaml:/opt/pyCPT/config.yaml \
#        --volume $(pwd)/plot.yaml:/opt/pyCPT/plot.yaml \
#        --detach cpt:latest

# CORRER MANUALMENTE PyCPT
# docker run --name pycpt --rm \
#        --volume $(pwd)/input:/opt/pyCPT/input \
#        --volume $(pwd)/output:/opt/pyCPT/output \
#        --volume $(pwd)/plots:/opt/pyCPT/plots \
#        --volume $(pwd)/config.yaml:/opt/pyCPT/config.yaml \
#        --volume $(pwd)/plot.yaml:/opt/pyCPT/plot.yaml \
#        cpt:latest /usr/local/bin/python /opt/pyCPT/main.py

# CORRER MANUALMENTE plot.R
# docker run --name pycpt --rm \
#        --volume $(pwd)/input:/opt/pyCPT/input \
#        --volume $(pwd)/output:/opt/pyCPT/output \
#        --volume $(pwd)/plots:/opt/pyCPT/plots \
#        --volume $(pwd)/config.yaml:/opt/pyCPT/config.yaml \
#        --volume $(pwd)/plot.yaml:/opt/pyCPT/plot.yaml \
#        cpt:latest /usr/bin/Rscript /opt/pyCPT/plot.R
