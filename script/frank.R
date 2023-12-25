################################################################
#rm(list=ls())
################################################################
library(dplyr)
library(ggplot2)
library(purrr)
library(ManifoldDestiny)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
################################################################
countys <- seq(1,88)
polinclude <- c(1,2,6)
polreport <- 3
tdf <- Voterdatabaseplots()
tdf$regvbase()
tdf$scorecard()
tdf$predictinput()
tdf$plot_predict()
tdf$plot_keyrat()
tdf$plot_histio()
tdf$gridarrange()
