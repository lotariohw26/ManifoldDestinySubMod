library(plumber)
filp <- paste0(rprojroot::find_rstudio_root_file(),"/inst/api/api.R")
#filb <- system.file("/api/api.R",package="ManifoldDestiny")
pr(filp) %>% pr_run(port=2000)
