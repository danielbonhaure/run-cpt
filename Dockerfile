
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

# Set CPT HOME
ARG CPT_HOME="/opt/CPT"

# Set python version
ARG PYTHON_VERSION="3.12"

# Set PyCPT HOME
ARG PyCPT_HOME="/opt/pyCPT"

# Set Pycharm version
ARG PYCHARM_VERSION="2023.1"

# Set global CRON args
ARG CRON_TIME_STR="0 0 16 * *"


##########################
## Stage 1: Compile CPT ##
##########################

# Create image
FROM debian:bullseye-slim AS cpt_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew CPT ARGs
ARG CPT_VERSION
ARG CPT_HOME

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
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
RUN mkdir $CPT_HOME

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
RUN mkdir $CPT_HOME/15.6.3
RUN mkdir $CPT_HOME/15.6.3/src
RUN mv /tmp/CPT/15.6.3/* $CPT_HOME/15.6.3/src/
# Mover código versión 15.7.11 a src
RUN mkdir $CPT_HOME/15.7.11
RUN mkdir $CPT_HOME/15.7.11/src
RUN mv /tmp/CPT/15.7.11/* $CPT_HOME/15.7.11/src/
# Mover código versión 16.5.8 a src
RUN mkdir $CPT_HOME/16.5.8
RUN mkdir $CPT_HOME/16.5.8/src
RUN mv /tmp/CPT/16.5.8/* $CPT_HOME/16.5.8/src/
# Mover código versión 17.3.1 a src
RUN mkdir $CPT_HOME/17.3.1
RUN mkdir $CPT_HOME/17.3.1/src
RUN mv /tmp/CPT/17.3.1/* $CPT_HOME/17.3.1/src/

# Instalar versión 15.6.3
# Acceder al código fuente
WORKDIR $CPT_HOME/15.6.3/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=$CPT_HOME/15.6.3/ install

# Instalar versión 15.7.11
# Acceder al código fuente
WORKDIR $CPT_HOME/15.7.11/src
# Para poder usar gfortran 10
RUN sed -i "s/-frecursive/-frecursive -fallow-argument-mismatch/g" ./lapack/lapack/make.inc
# Compilar e instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=$CPT_HOME/15.7.11/ install

# Instalar versión 16.5.8
# Acceder al código fuente
WORKDIR $CPT_HOME/16.5.8/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=$CPT_HOME/16.5.8/ install

# Instalar versión 17.3.1
# Acceder al código fuente
WORKDIR $CPT_HOME/17.3.1/src
# Compilar es instalar CPT
RUN make distclean
RUN make
RUN make INSTALL_DIR=$CPT_HOME/17.3.1/ install

# Configurar version por defecto para CPT
RUN echo "export CPT_BIN_DIR=$CPT_HOME/$CPT_VERSION/bin" >> /root/.bashrc
RUN echo "export PATH=$CPT_HOME/$CPT_VERSION/bin:$PATH" >> /root/.bashrc



######################################
## Stage 2: Install Python packages ##
######################################

# Create image
FROM python:${PYTHON_VERSION}-slim-bullseye AS py_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Set python environment variables
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # to install cartopy
        proj-bin libproj-dev libgeos-dev \
        # to install rpy2
        r-base r-base-dev && \
    rm -rf /var/lib/apt/lists/*

# Set work directory
WORKDIR /usr/src/app

# Upgrade pip and install dependencies
COPY requirements.txt /tmp/requirements.txt
RUN python3 -m pip install --upgrade pip && \
    python3 -m pip wheel --no-cache-dir --no-deps \
    --wheel-dir /usr/src/app/wheels -r /tmp/requirements.txt



#################################
## Stage 3: Install R packages ##
#################################

# Create image
FROM python:${PYTHON_VERSION}-slim-bullseye AS r_builder

# set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        build-essential \
        # install R
        r-base \
        # to install ncdf4
        libnetcdf-dev \
        # to install terra, a dependency of raster
        libgdal-dev libgeos-dev libproj-dev && \
    rm -rf /var/lib/apt/lists/*

# set CRAN mirror
ARG CRAN_MIRROR="getOption('repos')"

# install R packages
RUN R -e "options(warn=2); install.packages('ncdf4', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('terra', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('raster', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('dplyr', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"
RUN R -e "options(warn=2); install.packages('tibble', repos=${CRAN_MIRROR}, verbose=T, quiet=T, keep_outputs='/tmp/')"



#########################################
## Stage 4: Copy installation folders  ##
#########################################

# Create PyCPT image
FROM python:${PYTHON_VERSION}-slim-bullseye AS cpt-py-r_final

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew CPT ARGs
ARG CPT_VERSION
ARG CPT_HOME

# Renew PyCPT_HOME
ARG PyCPT_HOME

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
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
COPY --from=cpt_builder $CPT_HOME/$CPT_VERSION $CPT_HOME

# Setup CPT for root
RUN echo "export CPT_BIN_DIR=$CPT_HOME/bin" >> /root/.bashrc
RUN echo "export PATH=$CPT_HOME/bin:$PATH" >> /root/.bashrc

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



#################################
## Stage 5: Create PyCPT image ##
#################################

# Create PyCPT image
FROM cpt-py-r_final AS pycpt_builder

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew PyCPT_HOME
ARG PyCPT_HOME

# Create PyCPT_HOME folder
RUN mkdir -p $PyCPT_HOME

# Copy PyCPT code
COPY *.py $PyCPT_HOME/
COPY config.yaml $PyCPT_HOME/
COPY credentials.yaml.tmpl $PyCPT_HOME/

# Create input and output folders (these folders are too big so they must be used them as volumes)
RUN mkdir -p $PyCPT_HOME/input
RUN mkdir -p $PyCPT_HOME/output

# Save Git commit hash of this build into ${PyCPT_HOME}/repo_version.
# https://github.com/docker/hub-feedback/issues/600#issuecomment-475941394
# https://docs.docker.com/build/building/context/#keep-git-directory
COPY ./.git /tmp/git
RUN export head=$(cat /tmp/git/HEAD | cut -d' ' -f2) && \
    if echo "${head}" | grep -q "refs/heads"; then \
    export hash=$(cat /tmp/git/${head}); else export hash=${head}; fi && \
    echo "${hash}" > ${PyCPT_HOME}/repo_version && rm -rf /tmp/git

# Set permissions of app files
RUN chmod -R ug+rw,o+r,o-w $PyCPT_HOME && \
    chmod -R o+w $PyCPT_HOME/input $PyCPT_HOME/output



###########################################
## Stage 6: Install management packages  ##
###########################################

# Create image
FROM pycpt_builder AS pytcpt_mgmt

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # install Tini (https://github.com/krallin/tini#using-tini)
        tini \
        # to see process with pid 1
        htop procps \
        # to allow edit files
        vim \
        # to run process with cron
        cron && \
    rm -rf /var/lib/apt/lists/*

# Setup cron to allow it run as a non root user
RUN chmod u+s $(which cron)

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT ["/usr/bin/tini", "-g", "--"]



######################################
## Stage 7: Setup PyCPT core image  ##
######################################

# Create image
FROM pytcpt_mgmt AS pycpt-core

# Set environment variables
ARG DEBIAN_FRONTEND=noninteractive

# Renew CPT_HOME
ARG CPT_HOME

# Renew PyCPT_HOME
ARG PyCPT_HOME

# Renew CRON ARGs
ARG CRON_TIME_STR

# Install OS packages
RUN apt-get -y -qq update && \
    apt-get -y -qq upgrade && \
    apt-get -y -qq --no-install-recommends install \
        # to use envsubst
        gettext-base \
        # to configure locale
        locales \
        # to check container health
        redis-tools && \
    rm -rf /var/lib/apt/lists/*

# Configure Locale en_US.UTF-8
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    sed -i -e 's/# es_US.UTF-8 UTF-8/es_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales

# Set locale
ENV LC_ALL en_US.UTF-8

# Set read-only environment variables
ENV CPT_HOME=${CPT_HOME}
ENV PyCPT_HOME=${PyCPT_HOME}

# Set environment variables
ENV CRON_TIME_STR=${CRON_TIME_STR}
ENV CRCSAS_API_USR=
ENV CRCSAS_API_PWD=
ENV CopernicusCDS_API_URL="https://cds-beta.climate.copernicus.eu/api"
ENV CopernicusCDS_API_KEY=

# Crear archivo de configuración de CRON
RUN printf "\n\
\043 Setup cron to run files processor \n\
${CRON_TIME_STR} /usr/local/bin/python ${PyCPT_HOME}/main.py >> /proc/1/fd/1 2>> /proc/1/fd/1\n\
\n" > ${PyCPT_HOME}/crontab.conf
RUN chmod ug+rw,o+r,o-w ${PyCPT_HOME}/crontab.conf

# Crear archivo con variables de entorno
RUN touch ${PyCPT_HOME}/crontab-envvars.txt \
 && chmod ug+rw,o+r,o-w ${PyCPT_HOME}/crontab-envvars.txt

# CRON toma variables de entorno desde /etc/environment,
# para más info ver: https://askubuntu.com/a/700126
RUN mv /etc/environment /etc/environment-old \
 && ln -s ${PyCPT_HOME}/crontab-envvars.txt /etc/environment

# Setup CRON for root user
RUN (cat ${PyCPT_HOME}/crontab.conf) | crontab -

# Crear script de inicio/entrada. Este script debe reemplazar
# las variables de entorno definidas antes.
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
\043 Copiar variables de entorno del contenedor a /etc/environment \n\
xargs --null --max-args=1 --arg-file=/proc/1/environ > ${PyCPT_HOME}/crontab-envvars.txt \n\
\n\
\043 Crear archivo ${PyCPT_HOME}/credentials.yaml \n\
cat ${PyCPT_HOME}/credentials.yaml.tmpl | envsubst > ${PyCPT_HOME}/credentials.yaml \n\
chmod ug+rw,o+r,o-w ${PyCPT_HOME}/credentials.yaml \n\
\n\
\043 Borrar variables de entorno con credenciales \n\
unset CRCSAS_API_USR \n\
unset CRCSAS_API_PWD \n\
unset CopernicusCDS_API_URL \n\
unset CopernicusCDS_API_KEY \n\
\n\
\043 Reemplazar tiempo ejecución automática del procesador de archivos \n\
crontab -l | sed \"/main.py/ s|^\S* \S* \S* \S* \S*|\$CRON_TIME_STR|g\" | crontab - \n\
\n\
\043 Para correr CMD despues del entrypoint \n\
# Ver: https://stackoverflow.com/q/39082768 \n\
# Ver: https://stackoverflow.com/a/5163260 \n\
\043 OBS: exec $@ falla con comillas dobles \n\
exec \$@ \n\
\n" > /entrypoint.sh
RUN chmod a+x /entrypoint.sh

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
\n" > /check-healthy.sh
RUN chmod a+x /check-healthy.sh

# Add Tini (https://github.com/krallin/tini#using-tini)
ENTRYPOINT [ "/usr/bin/tini", "-g", "--", "/entrypoint.sh" ]

# Run your program under Tini (https://github.com/krallin/tini#using-tini)
CMD [ "cron", "-fL", "15" ]
# or docker run your-image /your/program ...

# Verificar si hubo alguna falla en la ejecución del replicador
HEALTHCHECK --interval=3s --timeout=3s --retries=3 CMD bash /check-healthy.sh



#####################################################
## Usage: Commands to Build and Run this container ##
#####################################################


# CONSTRUIR IMAGEN (CORE)
# docker build --force-rm \
#   --target pycpt-core \
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
