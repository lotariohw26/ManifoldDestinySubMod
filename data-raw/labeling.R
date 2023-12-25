################################################################################################################
library(ManifoldDestiny)
library(ggplot2)
library(purrr)
library(fredr)
library(DataEditR)
################################################################################################################
# Model
###############################################################################################################
fdm <- paste0(rprojroot::find_rstudio_root_file(),'/script/python/pysympy.py')
reticulate::source_python(fdm)
eqpar <- list(meql=reticulate::py$modeql,meqs=reticulate::py$modeqs)
## Saving data
usethis::use_data(eqpar, overwrite = TRUE)
################################################################################################################
# 20 laws and 40 isos
################################################################################################################
fdm <- paste0(rprojroot::find_rstudio_root_file(),'/script/python/20_laws_40_isos.py')
reticulate::source_python(fdm)
eqdef <- list(meql=reticulate::py$dfl,meqs=reticulate::py$dfs)
# Saving data
usethis::use_data(eqdef, overwrite = TRUE)
###############################################################################################################
# Saving data
###############################################################################################################
stickers <-
  list(parameters=list(
  standard=c("x","y","zeta","alpha","lamda"),
  hybrid=c("g","h","Gamma","alpha","Omega"),
  opposition=c("m","n","xi","lamda","Omega")),
  forms=list('_s','o_h','h_o')) 
usethis::use_data(stickers, overwrite = TRUE)
###############################################################################################################
# Cubic
###############################################################################################################
peqs <-c(
'w=k0 + k1*u + k2*v',
'w=k0 + k1*u + k2*v + k3*u**2 + k4*v**2 + k5*u*v',
'w=k0 + k1*u + k2*v + k3*u**2 + k4*v**2 + k5*u*v + k6*v**3 + k7*u*v + k8*u**2*v + k9*u*v**2',
'w=k0 + k1*u + k2*v + k3*u**2 + k4*v**2 + k5*u*v + k6*u**3 + k7*v**3 + k8*u**2*v + k9*u*v**2 + k10*u**4 + k11*v**4 + k12*u**3*v + k13*u**2*v**2 + k14*u*v**3 +k15*u**4*v + k16*u*v**2')
usethis::use_data(peqs, overwrite = TRUE)
###############################################################################################################
###############################################################################################################
