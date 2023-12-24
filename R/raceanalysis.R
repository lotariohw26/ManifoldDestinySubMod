# ' @export Raceanalysis
Raceanalysis <- setRefClass("Raceanalysis",fields=list(
							       sdfcs='data.frame',
							       descs='data.frame',
							       quints='data.frame', 
							       quints_rank='data.frame',
							       rpoltab='data.frame',
							       cogals='list',
							       rdfcs='list', 
							       parameters='list'
							       ))
Raceanalysis$methods(initialize=function(snap_df){

  abs_p <- rprojroot::find_rstudio_root_file()
  stickers <- ManifoldDestiny::stickers
  parameters <<- stickers[['parameters']]

  sn <- unique(snap_df$SNAP); 
  snl <- length(sn)
  rn <- unique(snap_df$RACENR)
  rnl <- length(rn)

  # Descriptive table
  str_vec_i <- c("S","T","U","V","Z")
  str_vec_s <- c("alpha")

  # I Adding total ballots
  descs <<- snap_df %>%
    dplyr::mutate(Z=S+T+U+V) %>%
    dplyr::group_by(SNAP,RACENR) %>%
    dplyr::summarize_at(vars(all_of(str_vec_i)), list(sum=sum,sd=sd)) %>%
    dplyr::arrange(RACENR,SNAP) %>%
    dplyr::mutate(alpha=(S_sum+U_sum)/Z_sum) %>%
    dplyr::mutate(x=(S_sum)/(S_sum+T_sum)) %>%
    dplyr::mutate(y=(U_sum)/(U_sum+V_sum)) %>%
    dplyr::mutate(Omega=(S_sum+T_sum)/Z_sum) %>%
    dplyr::group_by(RACENR) %>%
    dplyr::mutate(CZ=round(Z_sum/last(Z_sum),digits=4))

  # II  Changes
  sdfcs <<- snap_df %>%
    dplyr::left_join(dplyr::select(descs,SNAP,RACENR,CZ),by=c("SNAP","RACENR")) %>%
    dplyr::arrange(RACENR,P,SNAP) %>%
    dplyr::group_by(RACENR,P) %>%
    # Total ballots in precinct
    dplyr::mutate(ZP=S+T+U+V) %>%
    dplyr::mutate(CZP=ZP/last(ZP))  %>%
    # Change in ballots
    dplyr::mutate(dS=S-dplyr::lag(S)) %>%
    dplyr::mutate(dT=T-dplyr::lag(T)) %>%
    dplyr::mutate(dU=U-dplyr::lag(U)) %>%
    dplyr::mutate(dV=V-dplyr::lag(V)) %>%
    # Percentage change in ballots 
    dplyr::mutate(dSp=(S-dplyr::lag(S))/S)  %>%
    dplyr::mutate(dTp=(T-dplyr::lag(T))/T)  %>%
    dplyr::mutate(dUp=(U-dplyr::lag(U))/U)  %>%
    dplyr::mutate(dVp=(V-dplyr::lag(V))/V)  %>%
    dplyr::ungroup()

  # Instanting classes condition on race and snap 
  lapply(rn,function(rcn){
    sapply(sn,function(snp){
      sv <- c('PN','P','R','S','T','U','V','CZ')
      cou_inst <- sdfcs %>% dplyr::filter(RACENR==rcn,SNAP==snp) %>% Countinggraphs(selvar=sv)
      cou_inst$purging()
      cou_inst_pur <- Countinggraphs(cou_inst$rdfc,selvar=sv)
      cou_inst_pur$sortpre(form=1,polyn=9)
      cou_inst_pur$plot2d(1,labs=list(title=NULL,x="precinct (normalized)",y="percentage"))
      cou_inst_pur$plotxy(1)
      cou_inst_pur$plotly3d(partition=1)
      setNames(list(cou_inst_pur),rcn)
    }) -> snL
  }) -> rcL

  # Monster quint base table
  rpoltab <<- data.frame()
  for (rcn in 1:rnl) {
    for (snp in 1:snl) {
      ## Basic
      pvar <- rcL[[rcn]][[snp]]$rdfc %>% dplyr::select(P,PN,CZ,Z,S,T,U,V) %>% dplyr::mutate(SZ=(Z/sum(Z)))
      qui_rcn_snp <- rcL[[rcn]][[snp]]$quintile %>% 
        dplyr::mutate(RACENR=rcn,SNAP=snp) %>% 
        dplyr::left_join(pvar,by='P') %>%
        dplyr::relocate(SNAP) %>% 
        dplyr::relocate(RACENR) 
      quints <<- rbind(quints,qui_rcn_snp)
      
      ## Addendum
      rcL[[rcn]][[snp]]$sumreg[[1]]
      pln <- rcL[[rcn]][[snp]]$sumreg[[1]]
      vpol <- data.frame(race=rn[rcn],snap=sn[snp],
        		 polyn=rcL[[rcn]][[snp]]$sumreg[[2]], 
        		 polin=round(rcL[[rcn]][[snp]]$sumreg[[2]],5),
        		 r2=rcL[[rcn]][[snp]]$sumreg[[3]])
      rpoltab <<- rbind(rpoltab,vpol)
    }
  }  
  # Makin ranke index
  rso <- quints %>% 
          dplyr::filter(SNAP==rev(snl)[1]) %>%
          dplyr::select(P,PN,RACENR,alpha) %>%
          dplyr::group_by(RACENR) %>%
          dplyr::mutate(alpha_pri=row_number(alpha)) %>%
          dplyr::mutate(num=max(alpha_pri)) %>%
          dplyr::ungroup() %>%
          dplyr::rename(alpha_ind=alpha)

  # Graphical input table
  quints_rank <<- quints %>% 
    dplyr::left_join(dplyr::select(rso,-PN),by=c("P","RACENR")) %>%
    dplyr::arrange(RACENR,SNAP,alpha_ind) %>%
    dplyr::group_by(RACENR,SNAP) %>%
    dplyr::mutate(dev=row_number()-alpha_pri) %>%
    dplyr::ungroup()

  #openxlsx::write.xlsx(list(Ballots=sdfcs,Descriptive=descs,Quintiles=quints_rank,Sort=rso),paste0(abs_p,'/inst/addendum/xlsx/maricopa_monster.xlsx'))
})

Racegraphs <- setRefClass("Racegraphs", contains = c('Raceanalysis'))
Racegraphs$methods(plot2d=function(racenr=1,snap=1){

  psel <- c(unname(unlist(parameters))[c(1,2,4,10)])
  tab <- unique(dplyr::select(sdfcs,RACE,RACENR))
  quints_pl <- quints_rank %>% dplyr::left_join(tab,by='RACENR') %>% 
    dplyr::filter(RACENR==racenr,SNAP==snap) %>%
    tidyr::pivot_longer(all_of(c("alpha_ind",psel,paste0(psel,'_pred')))) 
 cnt <- unique(dplyr::select(snap_mar$quints,SNAP,RACENR,CZ)) %>% dplyr::filter(RACENR==racenr,SNAP==snap)

 go <- ggplot2::ggplot() + 
   ggplot2::geom_point(data=filter(quints_pl,name%in%c('alpha_ind',psel)),aes(x=alpha_pri,y=value, color=name)) +
   #ggplot2::geom_point(data=filter(quints_pl,name%in%c('alpha_ind',psel)),aes(x=alpha_pri,y=value,size=SZ, color=name)) +
   #ggplot2::geom_line(data=filter(quints_pl,name%in%paste0(psel[4],'_pred')),aes(x=pri,y=value, color=name))  + 
   ggplot2::scale_size(range = c(0.25, 2.0)) +
   ggplot2::ylim(0,1) +
   ggplot2::labs(title=paste0("Race",cnt[2], " Percentage counted: ",cnt[3]),x='Precinct (normalized)',y='Percentage',caption=NULL) +
   ggplot2::theme_bw()
   #plotly::ggplotly(go)

})
Racegraphs$methods(plot2dstatic_fac=function(){

  psel <- unname(unlist(parameters))[c(1,2,4,10)]
  tab <- unique(dplyr::select(sdfcs,RACE,RACENR))
  quints_pl <- quints_rank %>% dplyr::left_join(tab,by='RACENR') %>% tidyr::pivot_longer(all_of(c(psel,paste0(psel,'_pred')))) 
  
  go <- ggplot2::ggplot(quints_pl, aes(label=PN)) + 
    ggplot2::geom_point(data=filter(quints_pl,name%in%c(psel)),aes(x=alpha_pri,y=value, size=Z, color=name)) +
    #ggplot2::geom_line(data=filter(quints_pl,name%in%paste0(psel,'_pred')),aes(x=alpha_pri,y=value, color=name))  + 
    ggplot2::scale_size(range = c(0.25, 2.0)) +
    # Facet wrapping
    ggplot2::facet_wrap(vars(RACENR,CZ),nrow=2,labeller = label_both) +
    #ggplot2::facet_wrap(vars(RACENR,SNAP),nrow=2,labeller = label_both) +
    ggplot2::ylim(0,1) +
    ggplot2::labs(title=NULL,x='Precinct (normalized)',y='Percentage',caption=NULL) +
    ggplot2::theme_bw()
    plotly::ggplotly(go,tooltip=c("x","y","PN","Z"))
})
Racegraphs$methods(plot2ddynamic_fac=function(){

  psel <- unname(unlist(parameters))[c(1,2,4,10)]
  tab <- unique(dplyr::select(sdfcs,RACE,RACENR))
  quints_pl <- quints_rank %>% dplyr::left_join(tab,by='RACENR') %>% 
	  tidyr::pivot_longer(all_of(c("alpha_ind",psel,paste0(psel,'_pred')))) 

  ggplot2::ggplot() + 
    ggplot2::geom_point(data=filter(quints_pl,name%in%c('alpha_ind',psel)),aes(x=alpha_pri,y=value,size=Z, color=name)) +
     # Facet wrapping
     ggplot2::facet_wrap(vars(RACE),nrow=2) +
     ggplot2::scale_size(range = c(0.25, 2.0)) +
     ggplot2::ylim(0,1) +
     labs(title = 'Snap: {frame_time}', x ='Precinct (normalized)', y = 'Percentage') +
     gganimate::transition_time(SNAP) 
     #gganimate::ease_aes('linear') +
     #ggplot2::theme_bw()
})
