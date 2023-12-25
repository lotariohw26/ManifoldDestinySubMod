#######################################################################################################################################################
options(scipen=999)
set.seed(1)
library(kableExtra)
library(plotly)
library(dplyr)
library(ggplot2)
library(htmltools)
library(huxtable)
library(gridExtra)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc_py.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
md <- jsonlite::fromJSON(paste0(rprojroot::find_rstudio_root_file(),"/data-raw/metadata.json"))
dfm <- ManifoldDestiny::miller_stavros_nevada_2020[[1]] %>%
  dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>%
  dplyr::mutate(Z=S+T+U+V+A3+B3, Psi=Z/R) %>%
  dplyr::mutate(alpha=(S+U)/(Z)) %>% 
  dplyr::mutate(map=U/(S+T)) %>%
  dplyr::mutate(mbp=T/(T+V)) %>%
  dplyr::select(P,R,S,T,U,V)
#########################################################################################################################################################
##### Proto example 1 and 2
P  <- c(1,2,3,4,5,6)
S  <- c(60,60,60,60,60,55)
T  <- c(40,40,40,40,40,40)
U  <- c(40,40,40,40,40,40)
V  <- c(60,60,60,40,40,40)
Up <- c(40,(40-5),(40-10),40,40,40)
Vp <- c(60,(60+5),(60+10),40,40,40)
R  <- c(200,200,200,200,200,200)

## Tower 1
ex1m <- as.matrix(cbind(S,T,U,V))
towi <- as.data.frame(ex1m) %>% 
  dplyr::mutate(EDVst=paste0(.[,1],"/",.[,2])) %>%
  dplyr::mutate(MIVst_1=paste0(.[1,3],"/",.[1,4])) %>%
  dplyr::mutate(MIVst_2=paste0(.[2,3],"/",.[2,4])) %>%
  dplyr::mutate(MIVst_3=paste0(.[3,3],"/",.[3,4])) %>%
  dplyr::select(5:8) %>%
  t() %>%
  data.frame() %>%
  data.table::setnames(paste0("County_",1:6))

towi[3:4,1] <- ""
towi[4,2] <- ""

## Tower 2
ex2m <- as.matrix(cbind(S,T,Up,Vp))
towii <- as.data.frame(ex2m) %>% 
  dplyr::mutate(EDVst=paste0(.[,1],"/",.[,2])) %>%
  dplyr::mutate(MIVst_1=paste0(.[1,3],"/",.[1,4])) %>%
  dplyr::mutate(MIVst_2=paste0(.[2,3],"/",.[2,4])) %>%
  dplyr::mutate(MIVst_3=paste0(.[3,3],"/",.[3,4])) %>%
  dplyr::select(5:8) %>%
  t() %>%
  data.frame() %>%
  data.table::setnames(paste0("County_",1:6))
towii[3:4,1] <- ""
towii[4,2] <- ""
#########################################################################################################################################################
pro_rec_ex1_e <- data.frame(P=c(1,2,3,4,5,6),S=S,T=T,U=U,V=V,R=R)
pro_rec_ex2_e <- data.frame(P=c(1,2,3,4,5,6),S=S,T=T,U=Up,V=Vp,R=R) 
pro_rec_ex1 <- pro_rec_ex1_e[1:3,]
pro_rec_ex2 <- pro_rec_ex2_e[1:3,]
pro_elc_ex1 <- Countinggraphs(pro_rec_ex1,polyn=1)
pro_elc_ex2 <- Countinggraphs(pro_rec_ex2,polyn=1)
#pr_rep_1 <- seloutput <- seloutput(selreport(pro_elc_ex1,md$app4))
#pr_rep_2 <- seloutput <- seloutput(selreport(pro_elc_ex2,md$app4))
cx <- round(pro_elc_ex1$sdfc$x,2)
cy <- round(pro_elc_ex1$sdfc$y,2)
czeta <- round(pro_elc_ex1$sdfc$zeta,2)
calpha <- round(pro_elc_ex1$sdfc$alpha,2)
clambda <- round(pro_elc_ex1$sdfc$lamda,2)
cg <- round(pro_elc_ex1$sdfc$g,2)
ch <- round(pro_elc_ex1$sdfc$y,2)
cGamma <- round(pro_elc_ex1$sdfc$Gamma,2)
##########################################################################################################################################################
###### Normal 
######## R2 sim
probw <- c(m=0.51,s=0.10)
probva <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10)
probvb <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
ztech <- c(0,1)	
app_bal <- ballcastsim(dfm,probw,probva,probvb,ztech)
######### Normal form
app_n_rep <- selreport(app_bal,md$app0)
app_n_out <- seloutput(app_n_rep)
app_n_sim <- SimVoterdatabase(app_bal)
######## Rigged example 1: standard form
app_ex1_cou <- Countinggraphs(app_bal)
#print(app_ex1_cou$polyc[[1]][[1]])
app_ex1_cou$sortpre()
app_ex1_cou$mansys(sygen=list(frm=1,pre=c("alpha","x","y"),end=c("zeta","lamda"),me=c(plnr=1,rot=0)))
#app_ex1_cou$setres(NULL,0)
app_ex1_cou$setres(0.23,0)
app_ex1_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),TRUE,wn=c(0,0))
app_ex1_out <- seloutput(selreport(app_ex1_cou$rdfc,md$app0))
app_ex1_sim <- SimVoterdatabase(app_ex1_cou$rdfc)
######## Rigged example 2: hybrid form
app_ex2_cou <- Countinggraphs(app_bal)
pri_int_ex2 <- app_ex2_cou$polyc[[1]][[1]]
#print(app_ex2_cou$polyc[[1]][[1]])
app_ex2_cou$sortpre()
app_ex2_cou$mansys(sygen=list(frm=2,pre=c("alpha","g","h"),end=c("Gamma","Omega"),FALSE,me=c(plnr=1,rot=0)))
app_ex2_cou$setres(NULL,0)
app_ex2_cou$setres(0.23,0)
app_ex2_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),wn=c(0,0))
app_ex2_out <- seloutput(selreport(app_ex2_cou$rdfc,md$app0))
app_ex2_sim <- SimVoterdatabase(app_ex2_cou$rdfc)
###### Rigged example 3: Hybrid form
app_ex3_cou <- Countinggraphs(app_bal)
#pri_int_ex3 <- app_ex3_cou$polyc[[1]][[1]]
app_ex3_cou$sortpre()
app_ex3_cou$mansys(sygen=list(frm=1,pre=c("alpha","x","y"),end=c("zeta","lamda"),me=c(plnr=1,rot=0)))
app_ex3_cou$setres(NULL,0)
app_ex3_cou$setres(0.23,0)
app_ex3_cou$manimp(init_par=c(k0=0.0,k1=0.5,k2=0.5),wn=c(0,0))
app_ex3_out <- seloutput(selreport(app_ex3_cou$rdfc,md$app0))
app_ex3_sim <- SimVoterdatabase(app_ex3_cou$rdfc)
###################################################################################################################################################################
#### Standard
##################################################################################################################################################################
###### Dr. Frank
###countys <- seq(1,88)
###polinclude <- c(1,2,6)
###polreport <- 3
###tdf <- ManifoldDestiny::Voterrollreport()
###tdf$regvbase()
###tdf$scorecard()
###tdf$predictinput()
###############################################################################################################################################################
####### Concluding Tabl
ctone <-'Applications'
formved <- c('Opposition', 
	     'Opposition', 
	     'Normal', 
	     'Opposition')
casevec <- c('Miller vs. Stavros, 2020',
'Gilbert vs. Sisiolak vs. Lombardo 2020',
'Lake vs. Hobbs, 2022',
'Trump vs. Biden, 2020')
propv_vec <- c("1rd & 2th","1rd & 2th","1rd & 2th","1rd & 2th")
prope_vec <- c("5th & 6th","5th & 6th","3rd & 4th","5th & 6th")
cou_abc <- c("Case","Form of rigg","Properties","Violations")
concl_appps <- data.frame(case=casevec,
			  rig=formved,
			  propn=prope_vec, 
			  propv=propv_vec)
#################################################################################################################################################################
