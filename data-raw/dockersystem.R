# Save packages
ins_pack <- as.data.frame(installed.packages())[1:10,]
usethis::use_data(ins_pack, overwrite = TRUE)
#save(ins_pack,file="packages.RData")
# Load packages
#load(paste0(rprojroot::find_rstudio_root_file(),"/inst/Docker/packages.RData")); install.packages(ins_pack[1:3,1])
#RUN Rscript -e 'load(paste0(rprojroot::find_rstudio_root_file(),"/inst/Docker/packages.RData")); install.packages(ins_pack[1:3,1])'
#ask("load data from use_data in R?")

