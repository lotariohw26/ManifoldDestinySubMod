#test <- function(abc=NULL){
#  library(ManifoldDestiny)
#  library(ggplot2)
#  library(dplyr)
#  library(googlesheets4)
#  library(htmltools)
#  library(gridExtra)
#  md <- jsonlite::fromJSON(paste0(rprojroot::find_rstudio_root_file(),"/data-raw/metadata.json"))
#  googlesheets4::gs4_auth(email="lotariohw26@gmail.com")
#  lapply(paste0("app",2:2), function(x){recoudatr(mda=md[[x]])})
#}
#lapply(paste0("app",0:4), function(x){recoudatr(mda=md[[x]])})

#' @export recoudatr
recoudatr <- function(mda=NULL){
  gsh <- googlesheets4::read_sheet(mda$sht$url,sheet=mda$sht$pgn,range=mda$sht$rng) %>%
    data.table::setnames(new=mda$sht$cln) %>%
    dplyr::select(-starts_with('D')) %>%
    dplyr::mutate(P=row_number(PN)) %>%
    dplyr::mutate(R=row_number(RN)) %>%
    dplyr::mutate(S=!!rlang::parse_expr(mda$sht$stuv[1])) %>%
    dplyr::mutate(T=!!rlang::parse_expr(mda$sht$stuv[2])) %>%
    dplyr::mutate(U=!!rlang::parse_expr(mda$sht$stuv[3])) %>%
    dplyr::mutate(V=!!rlang::parse_expr(mda$sht$stuv[4]))
  assign(mda$nid,gsh)
  do.call("use_data", list(as.name(mda$nid), overwrite = TRUE))
  return(gsh)
}

#' @export gmp
gmp <- function(terms=c("x2","xy","y2","x3","x2y","y2x","y3")){
  # Preallocate a list to store expressions
  nrc <- as.character(rep(1:4))
  expre <- vector("list", length(terms))
  # Iterate over the terms
  for(i in seq_along(terms)){
    term <- terms[i]
    nl <- gsub("\\d","",term)
    chars <- strsplit(term, "")[[1]]
    ac <- paste0(chars,collapse="*")
    ind <- unlist(gregexpr("\\d",ac))
    sapply(1:nchar(nl), function(i){
      lcl <- regexpr(substring(nl,i,i), term)[[1]]
      cht <- substring(term,lcl,lcl+1)
      if (substring(cht,2,2) %in% nrc){
        aaa <- paste0(rep(substring(cht,1,1),as.numeric(substring(cht,2,2))),collapse="*")
      } 
      else{
       aaa  <- substring(cht,1,1)
      }
      aaa
  }) -> abc
  expre[i] <- paste0(abc,collapse="*")
  }
  expre
}

#' @export pareq
pareq <- function(ste='(x + y*zeta)/(zeta + 1)',lv=list(x=0.75,y=0.25,zeta=1)){
	eval(parse(text=ste),lv)
}
#' @export vpareq
vpareq <- function(dfr=NULL,enf=NULL,ste=NULL)({
  dfr %>% dplyr::mutate(!!enf:=pareq(ste=ste,lv=as.list(.[,]))) %>% dplyr::select(any_of(enf)) %>% as.vector()
})
#' @export me
me <- function(enfl=NULL,dfa=NULL){
   la <- enfl
   polyc <- setNames(as.vector(lapply(la, as.character)),LETTERS[1:5])
   la_e <- unlist(polyc[c(LETTERS[1:5])])
   pnr <- sum(la_e!="0") 
   ghi <- dfa %>%
   dplyr::mutate(A=pareq(la_e[1],c(as.list(.[,])))) %>%
   dplyr::mutate(B=pareq(la_e[2],c(as.list(.[,])))) %>%
   dplyr::mutate(C=pareq(la_e[3],c(as.list(.[,])))) %>% 
   dplyr::mutate(D=pareq(la_e[3],c(as.list(.[,])))) %>% 
   dplyr::mutate(E=pareq(la_e[3],c(as.list(.[,])))) %>%
   dplyr::group_by(P) %>%
   dplyr::mutate(polsolv=polysolver(pnr-1,c(A,B,C,D,E)[1:pnr])) %>%
   dplyr::mutate(!!paste0('y'):=Re(polsolv[1])) %>%
   dplyr::ungroup() %>%
   dplyr::select(y)
   #print(ghi$y)
   return(ghi$y)
}
#' Source specific lines in an R file
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.
##' @export source_lines
source_lines <- function(file, lines){
    source(textConnection(readLines(file)[lines]))
}
##' @export seloutput
seloutput <- function(selreport=NULL){
  tab0 <- selreport[[1]]$rdfc
  tab1 <- selreport[[1]]$desms
  tab2 <- selreport[[1]]$pl_corrxy[[1]]
  tab3 <- selreport[[1]]$pl_2dsort[[1]]
  tab4 <- selreport[[1]]$pl_3d_mani[[4]]
  tab5 <- selreport[[1]]$r2list
  tab6 <- list(summary(selreport[[2]]$regsum[[1]]))
  l1 <- selreport[[2]]$resplots[[1]][[1]]
  l2 <- selreport[[2]]$resplots[[1]][[2]]
  l3 <- selreport[[2]]$resplots[[1]][[3]]
  l4 <- selreport[[2]]$resplots[[1]][[4]]
  tab7 <- cowplot::plot_grid(plotlist=list(l1,l2,l3,l4))
  tab8 <- selreport[[2]]$comdesc
  tab9 <- selreport[[4]]
  tab10 <- selreport[[5]]$pl_2dsort
  tab11 <- selreport[[6]]
  list(rdfc=tab0,decs=tab1,corxy=tab2,qunt=tab3,ro3d=tab4,r2li=tab5,regr=tab6,resp=tab7,cmp=tab8,md=tab9,bb=tab10,md=tab11)
}
##' @export selreport
selreport <- function(
		      baldata=NULL,
		      md=NULL
		      ){

  frm <- md$mtd$sgs$fr
  rparv <- md$mtd$sgs$ro ; names(rparv) <- c("theta","phi","rho")
  co <- Countinggraphs(baldata)
  if (md$mtd$prg$cnd==1) co$purging(md$mtd,1)
  co$sortpre(frm)
  co$descriptive(frm)
  co$r2siminput(frm)
  co$plot2d(frm)
  co$plotxy(frm)
  co$resplot(frm)
  co$plotly3d(partition=frm)
  co$gridarrange()
  #co$rotation(selvar=c('Z','S','V'), 
  #	    rpar=rparv,
  #	    rs=c(1,4,2),
  #	    mmeanv=c(710.76471,257.67059,151.07059),
  #	    sli=50)
  #co$rotation(rpar=rparv)
  co$rotgraph()
  ges <- Estimation(co$rdfc,frm)
  ges$regression(md$mtd$sgs$eq)
  ges$hat_predict(md$mtd$sgs$va,as.numeric(md$mtd$sgs$fr))
  ges$diagnostics()
  #ges$hat_intcomp()
  ### Identify
  ies <- Estimation(co$rdfc,frm)
  ies$regression(md$mtd$sgs$eq)
  ies$diagnostics()
  ## Identify
  ### Bowplot
  cob <- Countinggraphs(baldata,selvar=names(baldata))
  cob$sortpre(4,3)
  cob$plot2d(4,labs=list(title=NULL,x="precinct (normalized)",y="percentage",caption=NULL,alpha=0.4,size=0.5))
  return(list(co=co,ges=ges,ies=ies,md=baldata[[2]],cb=cob,md=md))
}
# Initiating 
  #browser()
  #vc  <- c('alpha=k0+k1*x+k2*y+k3*zeta','alpha=k0+k1*g+k2*h+k3*Gamma','#!')[frm]
  #param <- ManifoldDestiny::stickers[['parameters']][[frm]]
  #chr <- unlist(strsplit(sugsol[1], ""))
  #ltr <- chr[grepl("[a-z]", chr)]
  #unique_ltr <- unique(ltr)
  #selv <- c(intersect(param,unique(ltr)),"alpha")
  ## Countinggraphs
  #co <- Countinggraphs(baldata[[1]],selvar=names(baldata[[1]]))
  #plt <- purge[seq(1,length(purge))]
  #if (!is.null(plt)) {do.call(co$purging,plt)}
  #co$sortpre(frm)
  #co$descriptive(frm)
  #co$r2siminput(frm)
  #co$plot2d(frm)
  #co$plotxy(frm)
  #co$resplot(frm)
  #co$plotly3d(partition=frm)
  #co$gridarrange()
  ##co$rotation(rpar=rparv)
  ##co$rotgraph()
  ## Estimating
  ### Guess
  #ges <- Estimation(co$rdfc,frm)
  #ges$regression(sugsol[1])
  #ges$hat_predict(sugsol[2],as.numeric(sugsol[3]))
  #ges$hat_intcomp()
  #ges$diagnostics()
  ### Identify
  #ies <- Estimation(co$rdfc,frm)
  #ies$regression(vc)
  #ies$diagnostics()
  ### Bowplot
  #cob <- Countinggraphs(baldata[[1]],selvar=names(baldata[[1]]))
  #cob$sortpre(4,3)
  #cob$plot2d(4,labs=list(title=NULL,x="precinct (normalized)",y="percentage",caption=NULL,alpha=0.4,size=0.5))
  #return(list(co=co,ges=ges,ies=ies,md=baldata[[2]],cb=cob))
#}
##' @export sympyupd
sympyupd <- function(){
	abs_path <- rprojroot::find_rstudio_root_file()
	fdm <- paste0(abs_path,'/inst/script/symbolic/pysympy.py')
	reticulate::source_python(fdm)
	eqpar <- list(meql=reticulate::py$modeql,meqs=reticulate::py$modeqs)
	usethis::use_data(eqpar, overwrite = TRUE)
}
##' @export bm
bm <- function(){
   devtools::document()
   system(paste0('cd ',rprojroot::find_rstudio_root_file(),'; R CMD INSTALL --preclean --no-multiarch --with-keep.source .'))
}
#' @export k
k <- function(){
    df <-  clipr::write_last_clip()
}
#' @export l
l <- function(){
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')

    temp_file <- paste0(tempfile(), '.xlsx')
    df <-  clipr::write_last_clip()
    openxlsx::write.xlsx(df, file = temp_file)
    invisible(system(paste(open_command, temp_file),
                     ignore.stdout = TRUE, ignore.stderr = TRUE))
}

#' @export ndf
ndf <- function(df=head(iris)){
  tf <- paste0(rprojroot::find_rstudio_root_file(),'/tmp.csv')
  write.csv(df,tf)
}
#library(animation)
#png_files <- dir(pattern = ".png$")
#saveGIF({
#  for (i in 1:length(png_files)) {
#    file <- png_files[i]
#    img <- readPNG(file)
#    plot(0,0,type='n',xlim=c(0,1),ylim=c(0,1),axes=F,ann=F)
#    rasterImage(img,0,0,1,1)
#  }
#}, interval = 0.5, movie.name = "animation.gif")

#dft <- data.frame(str=c("a","b"),a=1,b=2) %>% dplyr::mutate(abc=eval(parse(text(.[,"str"])))

#' @export runR2S
runR2S <- function() {
  appDir <- system.file("shinyapps/r2sim",package="ManifoldDestiny")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

