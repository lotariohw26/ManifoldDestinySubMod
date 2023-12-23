FROM rocker/rstudio
### DISTRO:Packages###
RUN apt-get update && apt-get install -y passwd git neovim python3 python3-pip openssh-server
#### DISTRO:Settings and file managments###
ARG USR=manifold
RUN useradd -ms /bin/bash
#RUN echo ${USR}:1234 | chpasswd
#ARG GIT_PAT_H
#RUN useradd -ms /bin/bash ${USR}
#RUN echo ${USR}:1234 | chpasswd
#WORKDIR /home/${USR}
#### R ###
#RUN R -e "install.packages(c('desc'), repos='https://cran.rstudio.com/')"
### settings
#RUN mkdir -p /home/${USR}
#WORKDIR /home/${USR}
#RUN git clone https://github.com/lotariohw26/ManifoldDestiny.git
#RUN chown -R ${USR}:${USR} /home/${USR}
#WORKDIR /home/${USR}/ManifoldDestiny
#RUN R -e 'install.packages(desc::desc_get_deps(file = ".")$package[-1])'
#RUN R CMD INSTALL --preclean --no-multiarch --with-keep.source .
#EXPOSE 8787
