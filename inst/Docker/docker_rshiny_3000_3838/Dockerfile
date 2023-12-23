## Use the rocker/shiny-verse image as the base image
FROM rocker/shiny-verse
### DISTRO:Packages###
RUN apt-get update && apt-get install -y passwd git neovim python3 python3-pip openssh-server
#### DISTRO:Settings and file managments###
ARG GIT_PAT_H
ARG USR=abc
RUN useradd -ms /bin/bash ${USR}
RUN echo ${USR}:1234 | chpasswd
WORKDIR /home/${USR}
RUN R -e "install.packages(c('desc'), repos='https://cran.rstudio.com/')"
RUN git clone https://github.com/lotariohw26/ManifoldDestiny.git
RUN R -e 'install.packages(desc::desc_get_deps(file = ".")$package[-1])'
RUN R CMD INSTALL --preclean --no-multiarch --with-keep.source .
EXPOSE 3838
#RUN mkdir /srv/shiny-server/myapp
##COPY app.R /srv/shiny-server/myapp/
#
## Set the working directory in the container
##WORKDIR /app
#
## Copy your Shiny app files to the container
##COPY . /app
#
## Set the working directory
#RUN mkdir -p /home/rstudio/ManifoldDestiny_ES
#RUN mkdir -p /home/rstudio/ManifoldDestiny_LH
#
## Manifold Destiny
#RUN git clone https://github.com/lotariohw26/ManifoldDestiny.git /home/rstudio/ManifoldDestiny_LH
#RUN chown -R rstudio:rstudio /home/rstudio
#RUN git config --global --add safe.directory /home/rstudio/ManifoldDestiny_LH
#WORKDIR /home/rstudio/ManifoldDestiny_LH
#RUN git pull
#RUN R -e "load('data/ins_pack.rda'); install.packages(ins_pack[,1])"
##RUN R CMD INSTALL --preclean --no-multiarch --with-keep.source .
#RUN pip3 install sympy
## Expose port 3838 for the Shiny app
#EXPOSE 3838
##
#### Start Caddy server with Shiny reverse proxy
##CMD ["/usr/local/bin/caddy", "--conf", "/etc/Caddyfile"]
## Start the Shiny app when the container is run
##CMD ["R", "-e", "shiny::runApp('/app')"]
#
### Set up the Shiny app directory
#
#
#
