##############################################################################################################################################################
options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
#library(gridextra)
# Application
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/r2simulation.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/snapshotanalysis.R'))
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
#############################################################################################################################################################
sel_ma <- 'k0+k1*x+k2*y'
app_ma_sel <- list(alpha=sel_ma,solvf='y',form=1)
app_ma_eqs <- list(alpha=c(set_n,sel_ma))
#############################################################################################################################################################
#############################################################################################################################################################
## Clark and Washoe
dfr <- ManifoldDestiny::lset
fir <- dfr[[1]] %>% dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>% Countinggraphs()
sec <- dfr[[2]] %>% dplyr::mutate(S=A3,T=B3,U=A1+A2,V=B1+B2) %>% Countinggraphs()
## Maricopa
canlet <- c("SNAP","RACE", "RACENR","TXT","P","PN","R","A1","B1","A2","B2")
thr <- data.table::rbindlist(ManifoldDestiny::lst_race_snap_all_az_ma,F)  %>% data.table::setnames(new=canlet) %>% dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>% dplyr::filter(SNAP==12,RACENR==2) %>% Countinggraphs()
bindf <- dplyr::select(rbind(fir$rdfc%>%dplyr::mutate(tpg='Nevada'),
			     sec$rdfc%>%dplyr::mutate(tpg='B'),
		             thr$rdfc%>%dplyr::mutate(tpg='Maricopa')),alpha,g,h,x,y,tpg)
#############################################################################################################################################################
#############################################################################################################################################################
## Clark and Washoe
## Sideeffect #1
plotly::plot_ly(bindf, x = ~g, y = ~h, z = ~alpha, color = ~tpg, text = ~tpg,
        type = "scatter3d", mode = "markers")
## Sideeffect #2
summary(lm('alpha~g+h',fir$rdfc))
summary(lm('V~Z+S',fir$rdfc))
plotlymat3d(dplyr::select(fir$rdfc,V,Z,T))
## Sideeffect #3
ggplot2::ggplot(fir$rdfc,aes(x=pri,y=lamda)) + geom_point() +geom_smooth(method = "lm", se = FALSE) + annotate("text", x = max(fir$rdfc$pri), y = max(fir$rdfc$lamda), 
           label = paste("y =", round(coef(lm(lamda ~ pri, data = fir$rdfc))[2], 2), 
                         "x +", round(coef(lm(lamda ~ pri, data = fir$rdfc))[1], 2)))



#############################################################################################################################################################
#############################################################################################################################################################
## Maricopa
## Sideeffect #1
plotly::plot_ly(bindf, x = ~x, y = ~y, z = ~alpha, color = ~tpg, text = ~ tpg, 
        type = "scatter3d", mode = "markers")
## Sideeffect #2
summary(lm('V~Z+S',thr$rdfc))
summary(lm('alpha~y+x',thr$rdfc))
plotlymat3d(dplyr::select(thr$rdfc,S,Z,T))
## Sideeffect #4
summary(lm('S~Z+T',thr$rdfc))
summary(lm('alpha~y+x',thr$rdfc))
## Sideeffect #4
thr$plot2d()
thr$sortpre()
thr$purging()
thr2<-Countinggraphs(thr$rdfc)
thr2$sortpre()
thr2$quintile
ggplot2::ggplot(thr2$quintile,aes(x=pri,y=Omega)) + geom_point() +geom_smooth(method = "lm", se = FALSE) + annotate("text", x = max(fir$rdfc$pri), y = max(fir$rdfc$Omega), 
           label = paste("y =", round(coef(lm(Omega ~ pri, data = fir$rdfc))[2], 2), 
                         "x +", round(coef(lm(Omega ~ pri, data = fir$rdfc))[1], 2)))
#############################################################################################################################################################
