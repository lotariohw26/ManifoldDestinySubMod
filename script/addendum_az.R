#############################################################################################################################################################
options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
#library(gridextra)
# Application
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/raceanalysis.R'))
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
#############################################################################################################################################################
sel_ma <- 'k0+k1*x+k2*y'
app_ma_sel <- list(alpha=sel_ma,solvf='y',form=1)
app_ma_eqs <- list(alpha=c(set_n,sel_ma))
#############################################################################################################################################################
##### Maricopa
###### "US Senate","MASTERS, BLAKE","KELLY, MARK"
canlet <- c("SNAP","RACE", "RACENR","TXT","P","PN","R","A1","B1","A2","B2")
df_m1 <- data.table::rbindlist(ManifoldDestiny::lst_race_snap_all_az_ma,F)  %>% data.table::setnames(new=canlet) %>% dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>% 
	dplyr::filter(RACENR%in%c(1)) %>% 
	dplyr::filter(SNAP%in%c(1:12))
### Facets
dyndf <- df_m1 
View(dyndf)
snap_mar <- Racegraphs(dyndf) 
filtered <- snap_mar$quints_rank %>% dplyr::filter(P==425)
#View(filtered)
### Dynamics
ssp <- unique(snap_mar$sdfcs$SNAP)
lr1 <- lapply(ssp,function(s){
  lgp <- snap_mar$plot2d(racenr=1,snap=s)
  fn <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/pres/race_1_snap",s,".png")
  ggplot2::ggsave(filename=fn)
  lgp
})
lr2 <- lapply(ssp,function(s){
  lgp <- snap_mar$plot2d(racenr=2,snap=s)
  fn <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/pres/race_2_snap",s,".png")
  ggplot2::ggsave(filename=fn)
  lgp
})
fdir <- paste0(rprojroot::find_rstudio_root_file(),"/inst/addendum/ggplots/pres/")
png_files <- base::dir(fdir)[c(1,2)]
animation::saveGIF({
  for (i in 1:length(png_files)) {
    file <- png_files[i]
    img <- png::readPNG(paste0(fdir,file))
    plot(0,0,type='n',xlim=c(0,1),ylim=c(0,1),axes=F,ann=F)
    rasterImage(img,0,0,1,1)
  }
}, interval = 0.5, movie.name = "animation.gif")
### Static
statdf <- df_m1 %>% dplyr::filter(SNAP==12,RACENR==2) 
df_mc <- Countingprocess(statdf)
#!
#app_ma_sen_arp <- allreport(df_mc$rdfc)
#app_ma_sen_srp <- sumreport(app_ma_sen_arp)
##################################################################################################################################################################
statdf2 <- statdf %>% dplyr::mutate(SNAP==12, RACENR==2) 
df_mc <- Countinggraphs(statdf2)
df_mc$purging(0,vfilter=list(S=0,T=0,U=0,V=0))
#df_mc$purging(0,vfilter=list(S=50,T=50,U=50,V=50))
df_mc2 <- Countinggraphs(df_mc$rdfc)
#df_mc2$sortpre()
#df_mc2$plot2d()
#mean(df_mc2$quintile$Omega)
#sd(df_mc2$quintile$Omega)
# [1] 0.06627117
#ggplot2::ggplot(df_mc2$quintile,aes(x=pri,y=Omega))+geom_point() + geom_smooth(method = "lm", se = FALSE) + ggpmisc::stat_poly_eq(formula = y ~ x, aes(label = paste("y =", ..eq.label..)), parse = TRUE)
###############################################################################################################################################################
#### Cohise
##### "US Senate","MASTERS, BLAKE","KELLY, MARK"
canlet <- c("SNAP","RACE","P","PN","R",do.call(c,lapply(c(1:3),function(x)paste0(LETTERS[x],seq(1,3)))))
df_co_all <- data.table::rbindlist(ManifoldDestiny::lst_race_snap_all_az_co,F) %>%
  dplyr::select(1:5,12:20) %>%
  data.table::setnames(new=canlet) %>%
  dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>%
  dplyr::select(SNAP,RACE,P,PN,R,S,T,U,V) %>%
  dplyr::mutate(RACENR=cumsum(!duplicated(RACE)), .after=SNAP)
# Particular race
df_cm <- Countingprocess(dplyr::filter(df_co_all,RACENR==1))
df_cm$purging(totv=0)
app_co_sen_arp <- allreport(df_cm$rdfc)
app_co_sen_srp <- sumreport(app_co_sen_arp)
# All races
sna_sta_all <- Racegraphs(df_co_all)
plo_sta_all <- sna_sta_all$plot2dstatic_fac()
#############################################################################################################################################################
#Before Nov 19, there were only about 2 destroyed ballots that are visible.
#>*** is a flag telling me the turnout and ballots don't match.
#Below are running totals. Dropped is a total of negatives precinct values between each snapshot.
#
#Nov 19
#Batch summary: Attorney General: turnout=247587 ballots=247567 dropped=-427 >***
#Batch summary: Clerk of the Superior Court: turnout=247587 ballots=244134 dropped=-398 >***
#Batch summary: County Attorney-Term Expires  DECEMBER 31, 2024: turnout=247587 ballots=247413 dropped=-435 >***
#Batch summary: Governor: turnout=247587 ballots=247537 dropped=-403 >***
#Batch summary: Secretary of State: turnout=247587 ballots=247309 dropped=-434 >***
#Batch summary: State Mine Inspector: turnout=247587 ballots=247168 dropped=-286>***
#Batch summary: State Treasurer: turnout=247587 ballots=247425 dropped=-434 >***
#Batch summary: Superintendent of Public Instruction: turnout=247587 ballots=247566 dropped=-424 >***
#Batch summary: US Rep Dist CD-1: turnout=55004 ballots=54917 dropped=-111 >***
#Batch summary: US Rep Dist CD-2: turnout=170 ballots=170 dropped=0
#Batch summary: US Rep Dist CD-3: turnout=23451 ballots=23407 dropped=-1 >***
#Batch summary: US Rep Dist CD-4: turnout=42700 ballots=42694 dropped=-130 >***
#Batch summary: US Rep Dist CD-5: turnout=45278 ballots=45272 dropped=-21 >***
#Batch summary: US Rep Dist CD-7: turnout=4051 ballots=4042 dropped=-3 >***
#Batch summary: US Rep Dist CD-8: turnout=48148 ballots=48002 dropped=-46 >***
#Batch summary: US Rep Dist CD-9: turnout=29180 ballots=29143 dropped=-24 >***
#Batch summary: US Senate: turnout=247982 ballots=247962 dropped=-414 >***
#
#Nov 21
#Batch summary: Attorney General: turnout=247675 ballots=247675 dropped=-1076
#Batch summary: Clerk of the Superior Court: turnout=247675 ballots=244235 dropped=-1028 >***
#Batch summary: County Attorney-Term Expires  DECEMBER 31, 2024: turnout=247675 ballots=247504 dropped=-1083 >***
#Batch summary: Governor: turnout=247675 ballots=247675 dropped=-1050
#Batch summary: Secretary of State: turnout=247675 ballots=247398 dropped=-1084 >***
#Batch summary: State Mine Inspector: turnout=247675 ballots=247675 dropped=-883
#Batch summary: State Treasurer: turnout=247675 ballots=247516 dropped=-1082 >***
#Batch summary: Superintendent of Public Instruction: turnout=247675 ballots=247675 dropped=-1070
#Batch summary: US Rep Dist CD-1: turnout=55126 ballots=55040 dropped=-117 >***
#Batch summary: US Rep Dist CD-2: turnout=170 ballots=170 dropped=0
#Batch summary: US Rep Dist CD-3: turnout=22968 ballots=22927 dropped=-579 >***
#Batch summary: US Rep Dist CD-4: turnout=42927 ballots=42927 dropped=-133
#Batch summary: US Rep Dist CD-5: turnout=45297 ballots=45297 dropped=-41
#Batch summary: US Rep Dist CD-7: turnout=4034 ballots=4025 dropped=-25 >***
#Batch summary: US Rep Dist CD-8: turnout=48259 ballots=48259 dropped=-52
#Batch summary: US Rep Dist CD-9: turnout=29289 ballots=29289 dropped=-31
#Batch summary: US Senate: turnout=248070 ballots=248070 dropped=-1063
