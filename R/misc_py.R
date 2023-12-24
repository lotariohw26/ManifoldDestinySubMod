#' @export py_genpolycoeff 
py_genpolycoeff <- function(expr=NULL,solvd=NULL,solvf=NULL,eur=c(0, 0, 0),dnr=0){
  #reticulate::source_python(system.file("www/script/sympy/functions.py",package = "ManifoldDestiny"))
  reticulate::source_python(paste0(rprojroot::find_rstudio_root_file(),"/inst/script/python/functions.py"))
  reticulate::py$genpolycoeff(expr=expr,solvd=solvd,solvf=solvf,eur=as.integer(eur),dnr=dnr)
}
#' @export py_polysolver
py_polysolver <- function(degree=1,kvec=NULL){
  path_fqs <- paste0(rprojroot::find_rstudio_root_file(),"/inst/script/python")
  #path_fqs <- system.file("www/script/python",package = "ManifoldDestiny")
  fqs <- reticulate::import_from_path("fqs", path =path_fqs)
  np <- reticulate::import("numpy")
  vec <- kvec[!is.na(kvec)] 
  if (degree==1) {
    retv <- np$roots(vec)[1]
  }
  if (degree==2) {
    retv <- np$roots(vec)[1]
  }
  if (degree==3) {
    retv <- fqs$cubic_roots(vec)[1]
  }
  if (degree==4) {
    retv <- fqs$quartic_roots(vec)[1]
  }
  retv
}
#reticulate::source_python(paste0(rprojroot::find_rstudio_root_file(),"/inst/www/script/python/functions.py"))
#padf <- as.data.frame(reticulate::py$genpolycoeff()[2])
#str(padf)
#padf <- as.data.frame(reticulate::py$padf())


