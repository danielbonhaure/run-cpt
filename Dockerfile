
##################################################################
##                           README                             ##
##################################################################
## Este Dockerfile permite crear un contendor con todos los pa- ##
## quetes y todas las configuraciones necesarias para calibrar  ##
## pronósticos utilizando CPT desde python (PyCPT).             ##
##################################################################



##########################
## Set GLOBAL arguments ##
##########################

# Set CPT version
ARG CPT_VERSION="15.7.11"

# Set CPT image variant
ARG CPT_IMG_VARIANT="bullseye-slim"

# Set CPT HOME
ARG CPT_HOME="/opt/CPT"

# Set python version
ARG PYTHON_VERSION="3.12"

# Set Python image variant
ARG IMG_VARIANT="-slim-bullseye"

# Set PyCPT HOME
ARG PyCPT_HOME="/opt/pyCPT"

# Set global CRON args
ARG CRON_TIME_STR="0 0 16 * *"



##########################
## Stage 1: Compile CPT ##
##########################

# Create image
FROM debian:${CPT_IMG_VARIANT} AS cpt_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew CPT ARGs
ARG CPT_VERSION
ARG CPT_HOME

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
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
RUN mkdir ${CPT_HOME}

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
RUN mkdir ${CPT_HOME}/15.6.3
RUN mkdir ${CPT_HOME}/15.6.3/src
RUN mv /tmp/CPT/15.6.3/* ${CPT_HOME}/15.6.3/src/
# Mover código versión 15.7.11 a src
RUN mkdir ${CPT_HOME}/15.7.11
RUN mkdir ${CPT_HOME}/15.7.11/src
RUN mv /tmp/CPT/15.7.11/* ${CPT_HOME}/15.7.11/src/
# Mover código versión 16.5.8 a src
RUN mkdir ${CPT_HOME}/16.5.8
RUN mkdir ${CPT_HOME}/16.5.8/src
RUN mv /tmp/CPT/16.5.8/* ${CPT_HOME}/16.5.8/src/
# Mover código versión 17.3.1 a src
RUN mkdir ${CPT_HOME}/17.3.1
RUN mkdir ${CPT_HOME}/17.3.1/src
RUN mv /tmp/CPT/17.3.1/* ${CPT_HOME}/17.3.1/src/

# Instalar versión 15.6.3
# Acceder al código fuente
WORKDIR ${CPT_HOME}/15.6.3/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=${CPT_HOME}/15.6.3/ install

# Instalar versión 15.7.11
# Acceder al código fuente
WORKDIR ${CPT_HOME}/15.7.11/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=${CPT_HOME}/15.7.11/ install

# Instalar versión 16.5.8
# Acceder al código fuente
WORKDIR ${CPT_HOME}/16.5.8/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=${CPT_HOME}/16.5.8/ install

# Instalar versión 17.3.1
# Acceder al código fuente
WORKDIR ${CPT_HOME}/17.3.1/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=${CPT_HOME}/17.3.1/ install

# Configurar version por defecto para CPT
RUN echo "export CPT_BIN_DIR=${CPT_HOME}/${CPT_VERSION}/bin" >> /root/.bashrc
RUN echo "export PATH=${CPT_HOME}/${CPT_VERSION}/bin:$PATH" >> /root/.bashrc



######################################
## Stage 2: Install Python packages ##
######################################

# Create image
FROM python:${PYTHON_VERSION}${IMG_VARIANT} AS py_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Set python environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        build-essential \
        # to install cartopy
        proj-bin libproj-dev libgeos-dev \
        # to install rpy2
        r-base r-base-dev && \
    rm -rf /var/lib/apt/lists/*

# Set work directory
WORKDIR /usr/src/app

# Upgrade pip and install dependencies
RUN python3 -m pip install --upgrade pip
# Copy dependencies from build context
COPY requirements.txt requirements.txt
# Install Python dependencies (ver: https://stackoverflow.com/a/17311033/5076110)
RUN python3 -m pip wheel --no-cache-dir --no-deps \
    --wheel-dir /usr/src/app/wheels -r requirements.txt



#################################
## Stage 3: Install R packages ##
#################################

# Create image
FROM python:${PYTHON_VERSION}${IMG_VARIANT} AS r_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        build-essential \
        # install R
        r-base \
        # to install ncdf4
        libnetcdf-dev \
        # to install terra, a dependency of raster
        libgdal-dev libgeos-dev libproj-dev && \
    rm -rf /var/lib/apt/lists/*

# Set CRAN mirror
ARG CRAN_MIRROR="getOption('repos')"

# Install R packages
RUN R -e "options(warn=2); install.packages('ncdf4', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('terra', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('raster', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('dplyr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tibble', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"



#########################################
## Stage 4: Copy installation folders  ##
#########################################

# Create PyCPT image
FROM python:${PYTHON_VERSION}${IMG_VARIANT} AS cpt-py-r_final

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Set python environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Renew CPT ARGs
ARG CPT_VERSION
ARG CPT_HOME

# Renew PyCPT ARGs
ARG PyCPT_HOME

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes upgrade && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # install R
        r-base \
        # to be able to use cartopy (Python)
        proj-bin libproj-dev libgeos-dev \
        # to be able to use rpy2 (R-Python)
        libblas-dev \
        # to be able to use ncdf4 (R)
        libnetcdf-dev \
        # to be able to use terra, a dependency of raster (R)
        libgdal-dev && \
    rm -rf /var/lib/apt/lists/*

# Copy CPT executable from cpt_builder
COPY --from=cpt_builder ${CPT_HOME}/${CPT_VERSION} ${CPT_HOME}

# Setup CPT for root
RUN echo "export CPT_BIN_DIR=${CPT_HOME}/bin" >> /root/.bashrc
RUN echo "export PATH=${CPT_HOME}/bin:$PATH" >> /root/.bashrc

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

# Set R libs paths (see: https://stat.ethz.ch/R-manual/R-devel/library/base/html/libPaths.html)
ENV R_LIBS="/usr/lib/R/library"
ENV R_LIBS_USER="/usr/local/lib/R/site-library"
ENV R_LIBS_SITE="/usr/local/lib/R/site-library"



##########################################
## Stage 5: Install management packages ##
##########################################

# Create image
FROM cpt-py-r_final AS base_image

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop procps \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Create utils directory
RUN mkdir -p /opt/utils

# Create script to load environment variables
RUN printf "#!/bin/bash \n\
export \$(cat /proc/1/environ | tr '\0' '\n' | xargs -0 -I {} echo \"{}\") \n\
\n" > /opt/utils/load-envvars

# Create startup/entrypoint script
RUN printf "#!/bin/bash \n\
set -e \n\
\043 https://docs.docker.com/reference/dockerfile/#entrypoint \n\
exec \"\$@\" \n\
\n" > /opt/utils/entrypoint

# Create script to check the container's health
RUN printf "#!/bin/bash \n\
exit 0 \n\
\n" > /opt/utils/check-healthy

# Set minimal permissions to the utils scripts
RUN chmod --recursive u=rx,g=rx,o=rx /opt/utils

# Allows utils scripts to run as a non-root user
RUN chmod u+s /opt/utils/load-envvars

# Setup cron to allow it to run as a non-root user
RUN chmod u+s $(which cron)

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]



#################################
## Stage 6: Create PyCPT image ##
#################################

# Create PyCPT image
FROM base_image AS pycpt_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # to save scripts PID
        # to check container health
        redis-tools && \
    rm -rf /var/lib/apt/lists/*

# Renew ARGs
ARG PyCPT_HOME

# Create PyCPT_HOME folder
RUN mkdir -p ${PyCPT_HOME}

# Copy PyCPT code
COPY *.py ${PyCPT_HOME}/
COPY config.yaml ${PyCPT_HOME}/
COPY credentials.yaml.tmpl ${PyCPT_HOME}/

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p ${PyCPT_HOME}/input
RUN mkdir -p ${PyCPT_HOME}/output

# Save Git commit hash of this build into ${PyCPT_HOME}/repo_version.
# https://github.com/docker/hub-feedback/issues/600#issuecomment-475941394
# https://docs.docker.com/build/building/context/#keep-git-directory
COPY ./.git /tmp/git
RUN export head=$(cat /tmp/git/HEAD | cut -d' ' -f2) && \
    if echo "${head}" | grep -q "refs/heads"; then \
    export hash=$(cat /tmp/git/${head}); else export hash=${head}; fi && \
    echo "${hash}" > ${PyCPT_HOME}/repo_version && rm -rf /tmp/git

# Set minimum required file permissions
RUN chmod -R u=rw,g=rw,o=r ${PyCPT_HOME} && \
    chmod -R u=rw,g=rw,o=rw ${PyCPT_HOME}/input && \
    chmod -R u=rw,g=rw,o=rw ${PyCPT_HOME}/output



#####################################
## Stage 7: Setup PyCPT core image ##
#####################################

# Create image
FROM pycpt_builder AS pycpt_core

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew ARGs
ARG CPT_HOME
ARG PyCPT_HOME
ARG CRON_TIME_STR

# Install OS packages
RUN apt-get --quiet --assume-yes update && \
    apt-get --quiet --assume-yes --no-install-recommends install \
        # to use envsubst
        gettext-base \
        # to configure locale
        locales && \
    rm -rf /var/lib/apt/lists/*

# Configure Locale en_US.UTF-8
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    sed -i -e 's/# es_US.UTF-8 UTF-8/es_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales

# Set locale
ENV LC_ALL en_US.UTF-8

# Create CRON configuration file
RUN printf "\n\
SHELL=/bin/bash \n\
BASH_ENV=/opt/utils/load-envvars \n\
\n\
\043 Setup cron to run files processor \n\
${CRON_TIME_STR} /usr/local/bin/python ${PyCPT_HOME}/main.py >> /proc/1/fd/1 2>> /proc/1/fd/1\n\
\n" > ${PyCPT_HOME}/crontab.conf

# Create startup/entrypoint script
RUN printf "#!/bin/bash \n\
set -e \n\
\n\
\043 Verificar que se hayan definido las credenciales necesarias \n\
if [ \${CRCSAS_API_USR:-'unset'} == 'unset' ]; then \n\
  echo 'Es obligatorio definir la variable de entorno CRCSAS_API_USR!' \n\
fi \n\
if [ \${CRCSAS_API_PWD:-'unset'} == 'unset' ]; then \n\
  echo 'Es obligatorio definir la variable de entorno CRCSAS_API_PWD!' \n\
fi \n\
if [ \${CopernicusCDS_API_KEY:-'unset'} == 'unset' ]; then \n\
  echo 'Es obligatorio definir la variable de entorno CopernicusCDS_API_KEY!' \n\
fi \n\
if [ \${CRCSAS_API_USR:-'unset'} == 'unset' ] || \
   [ \${CRCSAS_API_PWD:-'unset'} == 'unset' ] || \
   [ \${CopernicusCDS_API_KEY:-'unset'} == 'unset' ]; then \n\
  exit 1 \n\
fi \n\
\n\
\043 Crear archivo ${PyCPT_HOME}/credentials.yaml \n\
cat ${PyCPT_HOME}/credentials.yaml.tmpl | envsubst > ${PyCPT_HOME}/credentials.yaml \n\
chmod u=rw,g=rw,o=r ${PyCPT_HOME}/credentials.yaml \n\
\n\
\043 Borrar variables de entorno con credenciales \n\
unset CRCSAS_API_USR \n\
unset CRCSAS_API_PWD \n\
unset CopernicusCDS_API_URL \n\
unset CopernicusCDS_API_KEY \n\
\n\
\043 Reemplazar tiempo ejecución automática del procesador de archivos \n\
sed -i \"s|^\d\S+\s\S+\s\S+\s\S+\s\S+\s|\$CRON_TIME_STR|g\" /opt/utils/crontab.conf \n\
crontab -l | sed \"/main.py/ s|^\d\S+\s\S+\s\S+\s\S+\s\S+\s|\$CRON_TIME_STR|g\" | crontab - \n\
\n\
exec \"\$@\" \n\
\n" > /opt/utils/entrypoint

# Create script to check container health
RUN printf "#!/bin/bash\n\
if [ \$(find ${PyCPT_HOME} -type f -name '*.pid' 2>/dev/null | wc -l) != 0 ] || \n\
   [ \$(echo 'KEYS *' | redis-cli -h \${REDIS_HOST} 2>/dev/null | grep -c pycpt) != 0 ] && \n\
   [ \$(ps -ef | grep -v 'grep' | grep -c 'python') == 0 ] \n\
then \n\
  exit 1 \n\
else \n\
  exit 0 \n\
fi \n\
\n" > /opt/utils/check-healthy

# Set minimal permissions to the new scripts and files
RUN chmod u=rw,g=r,o=r ${PyCPT_HOME}/crontab.conf

# Set read-only environment variables
ENV CPT_HOME=${CPT_HOME}
ENV PyCPT_HOME=${PyCPT_HOME}

# Set user-definable environment variables
ENV CRON_TIME_STR=${CRON_TIME_STR}

# Set mandatory environment variables
ENV CRCSAS_API_USR=
ENV CRCSAS_API_PWD=
ENV CopernicusCDS_API_URL="https://cds-beta.climate.copernicus.eu/api"
ENV CopernicusCDS_API_KEY=

# Declare optional environment variables
ENV REDIS_HOST=localhost



######################################
## Stage 8: Setup PyCPT final image ##
######################################

# Create image
FROM pycpt_core AS pycpt-root

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew ARGs
ARG PyCPT_HOME

# Setup CRON for root user
RUN (cat ${PyCPT_HOME}/crontab.conf) | crontab -

# Create standard directories used for specific types of user-specific data, as defined 
# by the XDG Base Directory Specification. For when "docker run --user uid:gid" is used.
# OBS: don't forget to add --env HOME=/home when running the container.
RUN mkdir -p /home/.local/share && \
    mkdir -p /home/.cache && \
    mkdir -p /home/.config
# Set permissions, for when "docker run --user uid:gid" is used
RUN chmod -R a+rwx /home/.local /home/.cache /home/.config

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT [ "/usr/bin/tini", "-g", "--", "/opt/utils/entrypoint" ]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD [ "cron", "-fL", "15" ]
# or docker run your-image /your/program ...

# Configurar verificación de la salud del contenedor
HEALTHCHECK --interval=3s --timeout=3s --retries=3 CMD bash /opt/utils/check-healthy

# Set work directory
WORKDIR ${PyCPT_HOME}



#####################################################
## Usage: Commands to Build and Run this container ##
#####################################################


# CONSTRUIR IMAGEN (CORE)
# docker build --force-rm \
#   --target pycpt-root \
#   --tag ghcr.io/danielbonhaure/run-cpt:pycpt-core-v1.0 \
#   --build-arg CRON_TIME_STR="0 0 16 * *" \
#   --file Dockerfile .

# LEVANTAR IMAGEN A GHCR
# docker push ghcr.io/danielbonhaure/run-cpt:pycpt-core-v1.0

# CORRER OPERACIONALMENTE CON CRON
# docker run --name pycpt \
#   --mount type=bind,src=$(pwd)/input,dst=/opt/pyCPT/input \
#   --volume $(pwd)/output,dst=/opt/pyCPT/output \
#   --mount type=bind,src=.env \
#   --detach ghcr.io/danielbonhaure/run-cpt:pycpt-core-v1.0

# CORRER MANUALMENTE
# docker run --name pycpt \
#   --mount type=bind,src=$(pwd)/input,dst=/opt/pyCPT/input \
#   --mount type=bind,src=$(pwd)/output,dst=/opt/pyCPT/output \
#   --mount type=bind,src=$(pwd)/config.yaml,dst=/opt/pyCPT/config.yaml \
#   --env-file .env \
#   --rm ghcr.io/danielbonhaure/run-cpt:pycpt-core-v1.0 \
# python /opt/pyCPT/main.py --year 2023 --month 6
