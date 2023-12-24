library(ggplot2)
library(dplyr)
library(ManifoldDestiny)
library(googlesheets4)
library(htmltools)
library(gridExtra)
abs_p <- rprojroot::find_rstudio_root_file()
################################################################################################################
################################################################################################################
#####  s,t,u,v= Fritz EDV   ,  Ferry EDV,   Fritz Early+Mail    ;   Ferry Early+Mail,   for Will County                  , 2022 in blue.
sheets <- c(
'https://docs.google.com/spreadsheets/d/14ZwaFZw3YR05o8pgBj9ZcJEG0JC98-HFAFFLGeKYc_Y/edit#gid=0', 
'https://docs.google.com/spreadsheets/d/1lMP71vGUFVXNkz8L9zu9sL5afxjjd8lNRgwu_YnZV6w/edit#gid=0', 
'https://docs.google.com/spreadsheets/d/126bhidp4wbqf-2GBYjcx3TMIbV4u_9SnaeeS74QRcgA/edit#gid=145154006',
'https://docs.google.com/spreadsheets/d/1WszQbDEwF60ZehpF_MWBr8iEDYEWcOBZJwwbVv38sJU/edit#gid=0'
)
abc <- googlesheets4::read_sheet(sheets[1],sheet=1,range='A1:F1287') %>% data.table::setnames(new=c("P","R","A1","B1","A2","B2"))
def <- googlesheets4::read_sheet(sheets[2],sheet=1,range='A1:I311') %>% data.table::setnames(new=c("P","PN","R","A1","A2","A3","B1","B2","B3"))
lset <- list(abc,def)
usethis::use_data(lset, overwrite = TRUE)
devtools::document(); bm()
#############
#############
#############
#cou <- dft %>% data.table::setnames(new=c("P","R","S","T","U","V")) 
#
#
#Countinggraphs(cou)
#
#
#View(cou)
#sapply(sheets,function(sl){ browser()
#dft %>% DT::datatable()
#
#
#plot(gvisTable(dft))
#
#	dft <- googlesheets4::read_sheet(sl,sheet=1,range='A1:F1287')
#	View(dft)
#}) -> abcdef
#
#usethis::use_data(gov_sel,overwrite = TRUE)
#
#
#
#gs_ap_will <- 'https://docs.google.com/spreadsheets/d/1lMP71vGUFVXNkz8L9zu9sL5afxjjd8lNRgwu_YnZV6w/edit#gid=0'
#gs_ap_will_sh <- googlesheets4::read_sheet(gs_ap_will,sheet=1,range='A1:J311')
#gs_ap_will_sh_rec <- gs_ap_will_sh
###### s,t,u,v= Trump EDV,  Biden EDV,  Trump Mail            ;   Biden Mail           ,  for Philadelphia                  , 2020 in green.
#gs_ap_phil <- 'https://docs.google.com/spreadsheets/d/126bhidp4wbqf-2GBYjcx3TMIbV4u_9SnaeeS74QRcgA/edit#gid=145154006'
#usethis::use_data(gs_ap_phil, overwrite = TRUE)
#gs_ap_phil_sh_rec <- gs_ap_phil_sh
#usethis::use_data(gov_sel,overwrite = TRUE, sheet=3)
##### s,t,u,v= Trump EDV,  Biden EDV,  Trump Early+Mail ;   Biden Early+Mail,  for Maricopa                      , 2020 in red.
#gs_ap_mari <- 'https://docs.google.com/spreadsheets/d/1WszQbDEwF60ZehpF_MWBr8iEDYEWcOBZJwwbVv38sJU/edit#gid=0'
#usethis::use_data(mar_2022_sel,overwrite = T)
#####  s,t,u,v= Trump Early,  Biden Early,  Trump Mail            ;   Biden Mail           ,  for Clark+Washoe County , 2020 in purple.
#gs_ap_clwa <- 'https://docs.google.com/spreadsheets/d/14ZwaFZw3YR05o8pgBj9ZcJEG0JC98-HFAFFLGeKYc_Y/edit#gid=0'
#usethis::use_data(clark_washoe,overwrite = TRUE)
#################################################################################################################
#################################################################################################################
