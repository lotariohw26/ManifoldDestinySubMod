library(ManifoldDestiny)
library(ggplot2)
library(dplyr)
library(googlesheets4)
library(htmltools)
library(gridExtra)
library(usethis)
md <- jsonlite::fromJSON(paste0(rprojroot::find_rstudio_root_file(),"/data-raw/metadata.json"))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
googlesheets4::gs4_auth(email="lotariohw26@gmail.com")
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
lapply(paste0("app",4:4), function(x){recoudatr(mda=md[[x]])})
#2+2
#recoudatr(list(recn=recnav[1], 
  #      	 race='Presidential', 
  #      	 candidates=c('Trump (R)','Biden (D)'), 
  #      	 modes_of_voting=c("EV","MiV"), 
  #      	 state='Nevada', 
  #      	 county=c('Clark','Washoe'), 
  #      	 year=2020, 
  #      	 sug_sol=c('alpha=k0+k1*g+k2*h',solvf='g',root=1), 
  #      	 sht=c(ss="https://docs.google.com/spreadsheets/d/1gkf41sJRAQ6bwKmlAAcb7hAQ6v4zIXG2wDYl67LrJ4k/edit#gid=0",st=1,ra='A2:F1288'), 
  #      	 cln=c("PN","R","A1","B1","A2","B2"), bib=c("https://docs.google.com/spreadsheets/d/1gkf41sJRAQ6bwKmlAAcb7hAQ6v4zIXG2wDYl67LrJ4k/edit#gid=0", 
  #      	 "https://www.leg.state.nv.us/Statutes/69th/Stats199723.html#Stats199723page3471"), 
  #      	 recs=recnav
  #))
################################################################################################################
#recnav <- c('nevada_2020_presidential',
#	    'nevada_2020_clark_commissioner',
#	    'arizona_2022_governor',
#            'illinois_2022_clerk',
#            'georgia_2020_presidential',
#            'nevada_2022_secretary_of_state',
#            'nevada_2004_presidential',
#	    'nevada_2008_presidential',
#            'nevada_2022_secretary_of_state',
#	    'teexas_2022_presidential')[1:6]
#usethis::use_data(recnav, overwrite = TRUE)
#googlesheets4::gs4_auth(email="lotariohw26@gmail.com")
#################################################################################################################
#copy1 <- recoudatr(list(
#  recn=recnav[1],
#  race='Presidential',
#  candidates=c('Trump (R)','Biden (D)'),
#  modes_of_voting=c("EV","MiV"),
#  state='Nevada',
#  county=c('Clark','Washoe'),
#  year=2020,
#  sug_sol=c('alpha=k0+k1*g+k2*h',solvf='g',root=1),
#  sht=c(ss="https://docs.google.com/spreadsheets/d/1gkf41sJRAQ6bwKmlAAcb7hAQ6v4zIXG2wDYl67LrJ4k/edit#gid=0",st=1,ra='A2:F1288'),
#  cln=c("PN","R","A1","B1","A2","B2"),
#  bib=c("https://docs.google.com/spreadsheets/d/1gkf41sJRAQ6bwKmlAAcb7hAQ6v4zIXG2wDYl67LrJ4k/edit#gid=0",
#        "https://www.leg.state.nv.us/Statutes/69th/Stats199723.html#Stats199723page3471"),
#  recs=recnav
#))
#################################################################################################################
#copy2 <- recoudatr(list(
#  recn=recnav[2],
#  race='General Election',
#  candidates=c('Stavros (R)','Miller (D)'),
#  modes_of_voting=c("EV","MiV","EDV"),
#  state='Nevada',
#  county=c(''),
#  year=2020,
#  sug_sol=c('alpha=k0+k1*g+k2*g**2+k3*g*h+k4*h**2+k5*g**3+k6*g**2*h+k7*h**3',solvf='g',root=1),
#  sht=c(ss="https://docs.google.com/spreadsheets/d/1jvLhOzaaQUqmpt7XTz01DoLsLiXr3vNAw-79_PBCOsw/edit#gid=0",st='2',ra='A1:H151'),
#  cln=c("P","R","A1","B1","A2","B2","A3","B3"),
#  bib=c("https://www.documentcloud.org/documents/22088788-gilbert-v-sisolak-et-al"),
#  recs=recnav
#))
##################################################################################################################
#copy3 <- recoudatr(list(
#  recn=recnav[3],
#  race='l',
#  candidates=c('Lake (R)','Hobbs (D)'),
#  state='Maricopa',
#  county=c('Arizona'),
#  year=2022,
#  sug_sol=c('alpha=k0+k1*x+k2*y',solvf='y',root=1),
#  sht=c(ss="https://docs.google.com/spreadsheets/d/1FxJg9hjU-M1MIeKl0koiHDVIp2dPAmm3nJpRzd5Ejdg/edit#gid=301195549"
#,st='1',ra='G5:O940'),
#  cln=c("P","PN","R","A1","A2","B1","B2"),
#  bib=c()
#))
#################################################################################################################
#copy4 <- recoudatr(list(
#  recn=recnav[4],
#  race='l',
#  candidates=c('Fritz','Ferry'),
#  modes_of_voting=c("AdV","MiV","EDV","Prov"),
#  state='Illinois',
#  county=c('Will'),
#  year=2020,
#  sug_sol=c('alpha=k0+k1*h+k2*g+k3*h**2+k4*g*h+k5*g**2+k6*h**3+k7*h**2*g+k8*h*g**2+k9*g**3',solvf='g',root=1, 
#	    cntr='Solomon'),
#  sht=c(ss="https://docs.google.com/spreadsheets/d/1xN7lvxt1bHodRgHDk_wqJ9b1g3dZGSGKmSxIASPCGCI/edit#gid=301195549",st=3,ra='A5:K315'),
#  cld=c(4,5),
#  cln=c("C","PN","R","A1","A2","A3","B1","B2","B3"),
#  bib=c("https://docs.google.com/document/d/1ywNTOUZuJNB_viiwNuj029HZSzJEJdr7-2AY6n5SKsk/edit",
#	"https://docs.google.com/document/d/16Drw-NchWy3X3cOO1QheJdlY2Aprk3sWu7DQl3Vifcg/edit#heading=h.mbrhkokjyhxb")
#))
##################################################################################################################
##################################################################################################################
### Atlanta
#copy5 <- recoudatr(list(
#  recn=recnav[5],
#  race='Presidential',
#  candidates=c('Trump (R)','Biden (D)','Jorg (I)'),
#  state='Georgia',
#  county=c('Clayton','Cobb','...'),
#  year=2020,
#  sug_sol=c('alpha=k0+k1*g+k2*h',solvf='g',root=1),
#  sht=c(ss="https://docs.google.com/spreadsheets/d/1HoOpkuWDnfTG-mezrCBd7t04gb0YkhA5YDMzcgK-MVo/edit#gid=0",st=1,ra='B1:Q1025'),
#  cln=c("P","C","PN","R","A1","B1","A2","B2","A3","B3","C1","C2","C3","A4","B4","C4"),
#  bib=c("https://docs.google.com/document/d/1X90y8zuBWI0NW2pFNjB77VcZIyBo-W4RiV1mb8BsMJE/edit")
#))
##################################################################################################################
### neutral
#copy6 <- recoudatr(list(
#  recn=recnav[6],
#  race='',candidates=c('Marchant (R)','Cisco (D)'),
#  state='',
#  county=c(''),
#  year=2022,
#  sug_sol=c('alpha=NULL',solvf=NULL,root=1),
#  sht=c(ss='https://docs.google.com/spreadsheets/d/1gnUfOux-3U07YtuPHfuP-jmpPutp3WBVJ_gQ-9ZTr1k/edit#gid=811418100',st='5',range='A5:Q634'),
#  cld=c(4:11),
#  cln=c("C","P","R","A1","A2","A3","B1","B2","B3"),
#  bib=c("")
#))
##################################################################################################################
### neutral
##copy8 <- recoudatr(list(
##  recn=recnav[7],
##  race='Presidential',
##  candidates=c('Mcain (r)','Obama (d)'),
##  state='',
##  county=c(''),
##  year=2008,
##  sug_sol=c('fair election'),
##  sht=c(ss="https://docs.google.com/spreadsheets/d/1v9-bAI9INnjgfInEJBHNBp4D7N3-Opf4nDKJbzxdBrk/edit#gid=1531166097u",st=2,ra='A1:I782'),
##  cln=c("R","P","PN","A1","B2","A2","B2","A3","B3"),
##  bib=c()
##))
##copy6 <- recoudatr(list(
##  recn=recnav[6],
##  race='l',
##  candidates=c('',''),
##  state='Nevada',
##  county=c(''),
##  year=2020,
##  sug_sol=c('FAIR',solvf=NULL,root=NULL),
##  sht=c(ss="https://docs.google.com/spreadsheets/d/1DC5zZjoklVgTNExXRTxAIXYcq8_TpVn_WAK03pi6Fpw/edit#gid=0",st='2',ra='A1:I218'),
##  cln=c("R","P","PN","A1","B1","A2","B2","A3","B3"),
##  bib=c()
##))
##################################################################################################################
###################################################################################################################
##copy9 <- recoudatr(list(
##  recn=recnav[8],
##  race='',candidates=c('Marchant (R)','Cisco (D)'),
##  state='',
##  county=c(''),
##  year=2022,
##  sug_sol=c(alpha=NULL,solvf=NULL,root=1),
##  sht=c(ss='https://docs.google.com/spreadsheets/d/1gnUfOux-3U07YtuPHfuP-jmpPutp3WBVJ_gQ-9ZTr1k/edit#gid=811418100',st='5',range='A5:Q634'),
##  cld=c(4:11),
##  cln=c("C","P","R","A1","A2","A3","B1","B2","B3"),
##  bib=c("")
##))
##copy10 <- recoudatr(list(
##  recn=recnav[10],
##  race='Presidential',
##  candidates=c('Bush (R)','Kerry (D)'),
##  state='Texas',
##  county=c(''),
##  year=2004,
##  sug_sol=c("Fair"),
##  sht=c(ss='https://docs.google.com/spreadsheets/d/1DC5zZjoklVgTNExXRTxAIXYcq8_TpVn_WAK03pi6Fpw/edit#gid=0',st=2,range='A1:H611'),
##  cln=c("P","R","A1","B1","A2","B2","A3","B3"),
##  bib=c("")
##))
##################################################################################################################
#################################################################################################################
#################################################################################################################
##### Historial data
##hdt <- c('https://docs.google.com/spreadsheets/d/1xyMQovYh81Wptz-5fyVuLpL4uHrNE78NCDKOVQZ8MyE/edit#gid=0',
##'https://docs.google.com/spreadsheets/d/1v9-bAI9INnjgfInEJBHNBp4D7N3-Opf4nDKJbzxdBrk/edit#gid=0',
##'https://docs.google.com/spreadsheets/d/1pzpaOrABQ_9oJK5juubeG_gj4InJpkFzhi5yzxwqf8k/edit#gid=0', 
##'https://docs.google.com/spreadsheets/d/1Gs2Z3eo-ZNO7FZRGXZpgh6J2dJeyryCaMLSbUg3tQig/edit#gid=0', 
##'https://docs.google.com/spreadsheets/d/1DC5zZjoklVgTNExXRTxAIXYcq8_TpVn_WAK03pi6Fpw/edit#gid=0',
##'https://docs.google.com/spreadsheets/d/1tWgYZcv53teugdQi4WYmNEw0XlIBktrXqka8G6I7TJk/edit#gid=0',
##'https://docs.google.com/spreadsheets/d/1tAB6sjG7P-0y6PzxyYDvKMbYDK833MgQhfLti5Ip6Gg/edit#gid=1217173103',
##'https://docs.google.com/spreadsheets/d/1xk8IW9v03i0omZfUo9c75_7wuw9RvE1w8UqUyo1WzAw/edit#gid=1217173103')
###################################################################################################################
