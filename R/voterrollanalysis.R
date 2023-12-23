#' @export SimpleVoterdatabase
SimpleVoterdatabase <- setRefClass("SimpleVoterdatabase",fields=list(
  listcbase='data.frame',prob_df='data.frame'
								     ))
SimpleVoterdatabase$methods(initialize=function(sim_df=123, 
					        probw=c(0.5,0), 	
					        probva=c(0.5,02,0.01,0.01), 
					        probvb=c(0.5,02,0.01,0.01), 
					        ztech=c(0,1)
						){

  prob_df <<- sim_df %>%
    dplyr::mutate(WP=rnorm(n(),probw[1],probw[2])) %>%
    dplyr::mutate(N=runif(n(),ztech[1],ztech[2])) %>%
    dplyr::mutate(p3=(1-ztech)*(1-rnorm(n(),probva[1],probva[3]))) %>%
    dplyr::mutate(p6=(1-ztech)*(1-rnorm(n(),probvb[1],probvb[3]))) %>%
    dplyr::mutate(p2=(1-p3)*rnorm(n(),probva[2],probva[4])) %>%
    dplyr::mutate(p5=(1-p6)*rnorm(n(),probvb[2],probvb[4])) %>%
    dplyr::mutate(p1=1-p2-p3) %>%
    dplyr::mutate(p4=1-p5-p6) %>%
    dplyr::select(P,WP,N,p1,p2,p3,p4,p5,p6)

  listcbase <<- sim_df %>% dplyr::left_join(prob_df,by='P') %>% base::split(.$P) %>%
       purrr::map(function(x){
  # Assigning voters in each precinct
  cp <- stats::rbinom(x$Z,1,x$WP) 
  ## Setting up vector frame
  dc <- data.frame(P=x$P,WP=x$WP,R=x$R,C=cp,p1=x$p1,p2=x$p2,p3=x$p3,p4=x$p4,p5=x$p5,p6=x$p6) %>%
    dplyr::mutate(Id=row_number()) %>% 
    dplyr::relocate(Id,.before=P)  %>%
    dplyr::group_by(Id) %>% 
    dplyr::mutate(V=ifelse(C==1,sample(1:3,1,prob=c(p1,p2,p3)),sample(4:6,1,prob=c(p4,p5,p6)))) %>%
    dplyr::ungroup() 
  })  %>%
  dplyr::bind_rows(.) %>%
  dplyr::mutate(Id=row_number(P)) %>% 
  dplyr::relocate(Id,.before=P) %>% 
  dplyr::mutate(S=ifelse(V==1,1,0)) %>% 
  dplyr::mutate(T=ifelse(V==4,1,0)) %>%  
  dplyr::mutate(U=ifelse(V==2,1,0)) %>%
  dplyr::mutate(V=ifelse(V==5,1,0)) %>%
  dplyr::arrange(P) %>%  dplyr::group_by(P)  %>% 
  dplyr::select(c('P','R','S','T','U','V')) %>%
  dplyr::mutate(S=sum(S),T=sum(T),U=sum(U),V=sum(V)) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(Z=sum(S+T+U+V)) %>%
  dplyr::ungroup()
})

#' @export Voterdatabase
Voterdatabase <- setRefClass("Voterdatabase",fields=list(
  listvbase='list',
  listcbase='data.frame',
  voterroll='data.frame',
  predictsc='list', 
  listscard='list', 
  polyscard1='list',
  polyscard2='list',
  polypredi='list', 
  polcou='list',  
  lg_pred='list',  
  lg_hist='list',  
  lg_keyr='list', 
  pr_path='character')
)

Voterdatabase$methods(initialize=function(type_nr=2,lsv=1){

  elect_type <- c ('sim','rec')[type_nr]
  reciniload <- ManifoldDestiny::vtr_ohio
  pr_path <<- rprojroot::find_rstudio_root_file()
  votdf <- as.data.frame(reciniload) %>% dplyr::select(id,cou_nr,cou_na,age,prec_nr,registered,voted) 

  listvbase[[1]] <<- votdff <- as.data.frame(reciniload) %>% 
	  dplyr::select(cou_nr,cou_na,id,age,prec_nr,registered,voted) %>%
          dplyr::rename(P=prec_nr,R=registered,V=voted) %>%
          dplyr::select(cou_nr,cou_na,id,age,P,R,V) 

})

Voterdatabase$methods(regvbase=function(arg1=NULL){

  listvbase[[2]] <<- listvbase[[1]] %>% 
  dplyr::select(cou_nr,cou_na,id,age,P,R,V) %>%
  ##dplyr::mutate(V=R-C) %>%
  dplyr::group_by(cou_nr,age) %>%
  dplyr::arrange(cou_nr,age) %>%
  dplyr::mutate(ag_geovo=n_distinct(id)) %>% 
  dplyr::mutate(ag_voted=sum(V,na.rm=T)) %>%
  dplyr::mutate(ag_regis=sum(R)) %>% 
  dplyr::mutate(ag_gevos=ag_voted/ag_geovo) %>% 
  dplyr::mutate(ag_revos=ag_voted/ag_regis) %>% 
  # Total
  dplyr::ungroup() %>%
  dplyr::select(cou_nr,cou_na,age,ag_geovo,ag_regis,ag_voted,ag_gevos,ag_revos) %>%
  dplyr::distinct() %>%
  dplyr::mutate(tot_geopo=sum(ag_geovo)) %>%
  dplyr::mutate(tot_voted=sum(ag_voted)) %>%
  dplyr::mutate(tot_regis=sum(ag_regis)) %>%
  ## relationship between age and county
  dplyr::mutate(geo_ratio=tot_voted/tot_geopo) %>%
  dplyr::mutate(tur_ratio=tot_voted/tot_regis) %>%
  dplyr::mutate(go_key_ratio=ag_gevos/geo_ratio) %>%
  dplyr::mutate(re_key_ratio=ag_revos/tur_ratio) 
})
Voterdatabase$methods(scorecard=function(polyo=c(1,2,6,8)){
			      
  polcou[[1]] <<- polyo
  polcou[[2]] <<- unique(listvbase[[2]]$cou_nr)

  lapply(polcou[[2]] ,function(x){
    lapply(1:length(polyo),function(y){
      dft <- dplyr::filter(listvbase[[2]],cou_nr==x)
      #ft1 <- paste0("dft$go_key_ratio~poly(dft$age,",polyo[y],",raw=T)")
      #ft2 <- paste0("dft$re_key_ratio~poly(dft$age,",polyo[y],",raw=T)")
      ft1 <- paste0("dft$go_key_ratio~poly(dft$age,",polyo[y],",raw=T)")
      ft2 <- paste0("dft$re_key_ratio~poly(dft$age,",polyo[y],",raw=T)")
      list(lm(as.formula(ft1)),lm(as.formula(ft2)))
   })
    }) ->> listscard
    polyscard1 <<- lapply(1:length(polyo), function(x) sapply(1:length(polcou[[2]]), function(y) unname(listscard[[y]][[x]][[1]]$coeff)))	
    polyscard2 <<- lapply(1:length(polyo), function(x) sapply(1:length(polcou[[2]]), function(y) unname(listscard[[y]][[x]][[1]]$coeff)))	
})
Voterdatabase$methods(predictinput=function(arg1=NULL){

  polypredi <<- lapply(1:length(polcou[[1]]), function(x){

    avg_key_poly1 <- t(polyscard1[[x]]) %>% base::colMeans() %>% polynom::polynomial()
    avg_key_poly2 <- t(polyscard2[[x]]) %>% base::colMeans() %>% polynom::polynomial()
    predictsc[[1]] <<- listvbase[[2]] %>%
    dplyr::group_by(cou_nr) %>%
    dplyr::arrange(cou_nr) %>%
    ## Predicting average scorecard
    dplyr::mutate(polyo=polcou[[1]][x]) %>%
    dplyr::relocate(polyo) %>%
    dplyr::mutate(avg_key_ratio1=stats::predict(avg_key_poly1,age)) %>%
    dplyr::mutate(avg_key_ratio2=stats::predict(avg_key_poly2,age)) %>%
    dplyr::mutate(ag_vpred1=ag_geovo*geo_ratio*avg_key_ratio1) %>%
    dplyr::mutate(ag_vpred2=ag_regis*tur_ratio*avg_key_ratio2) %>%
    dplyr::mutate(pred_error1=ag_voted-ag_vpred1) %>%
    dplyr::mutate(pred_error2=ag_voted-ag_vpred2) %>%
    dplyr::mutate(corr1=cor(ag_voted,ag_vpred1)) %>%
    dplyr::mutate(corr2=cor(ag_voted,ag_vpred2)) %>%
    dplyr::ungroup()}
  ) 
  # For report
  #View(predictsc[[1]])
  predictsc[[2]]  <<- polypredi %>% dplyr::bind_rows(.) %>%
  dplyr::mutate(state='state') %>%
  dplyr::select(state,cou_nr,cou_na,polyo,corr1,corr2) %>% 
  dplyr::distinct()  
  # Agg report
  predictsc[[3]] <<- predictsc[[2]] %>% 
  dplyr::group_by(polyo) %>%
  dplyr::mutate(mcorr1=mean(corr1)) %>%
  dplyr::mutate(mcorr2=mean(corr2)) %>%
  dplyr::select(state,polyo,mcorr1,mcorr2) %>% 
  dplyr::distinct()
 
})
Voterdatabase$methods(uploadvbase=function(
				    truev=NULL, 
				    maniv=NULL, 
				    param=NULL 
				    ){
  listvbase[[3]] <<- listvbase[[2]] 
})

#' @export Voterdatabaseplots
Voterdatabaseplots <- setRefClass("Voterdatabaseplots", contains = c('Voterdatabase'))
Voterdatabaseplots$methods(plot_predict=function(plotyvar=c('ag_geovo','ag_voted','ag_regis','ag_vpred1','ag_vpred2'), lp=list(x='Age category',y='Number of voters') 
){
  for (po in 1:length(polcou[[1]])){
    lg_pred[[po]] <<- lapply(polcou[[2]], function(x){
      dfg <- polypredi[[po]] %>% dplyr::filter(cou_nr==x) %>% tidyr::pivot_longer(all_of(plotyvar)) 
      ctitle <- paste0('County: ',dfg$cou_na[x])
      cor1 <- round(unique(dfg$corr1), digits=5)
      cor2 <- round(unique(dfg$corr2), digits=5)
      captionp <- paste0('correlation 1 (r)=',cor1,' correlation 2 (r)=',cor2)
      lp <- ggplot2::ggplot() + 
	geom_line(data=dfg , aes(x=age,y=value,color=name)) + 
	ggplot2::labs(x=lp$x,y=lp$y) 
  })
}
})
Voterdatabaseplots$methods(plot_keyrat=function(plotyvar=list(li=c('avg_key_ratio1','avg_key_ratio2'),po=c('go_key_ratio','re_key_ratio','avg_key_ratio1','avg_key_ratio2','tur_ratio'))){

  for (po in 1:length(polcou[[1]])){
    lg_keyr[[po]] <<- lapply(polcou[[2]], function(x){
      dfg <- polypredi[[po]] %>% dplyr::filter(cou_nr==x)  %>% 
	      tidyr::pivot_longer(all_of(c(plotyvar$li,plotyvar$po)))
      ctitle <- paste0('County:',dfg$cou_na[x])
      lp <- ggplot2::ggplot() + 
	      geom_line(data=dplyr::filter(dfg,name%in%plotyvar$li), aes(x=age,y=value, color=name)) + 
	      geom_point(data=dplyr::filter(dfg,name%in%plotyvar$po), aes(x=age,y=value, color=name)) + scale_y_continuous(limits=c(0, 2)) 
    })
  }				  
})
Voterdatabaseplots$methods(plot_histio=function(plotyvar=c('pred_error1','pred_error2')){

 for (po in 1:length(polcou[[1]])){
   lg_hist[[po]] <<- lapply(polcou[[2]], function(x){
     dfg <- polypredi[[po]] %>% dplyr::filter(cou_nr==x) %>% tidyr::pivot_longer(all_of(plotyvar))
     ctitle <- paste0('County:',dfg$cou_na[x])
     lp <- ggplot2::ggplot(data=dfg) + 
	     geom_histogram(aes(x=value, fill=name),bins=30) 
   })
 }				      
})
Voterdatabaseplots$methods(gridarrange=function(arg1=NULL){

  nmlc <- unique(listvbase[[2]]$cou_na)
  for (lc in 1:length(nmlc)){
    # ls <- 1
    nmlcl <- nmlc[lc]
    gr1 <- lg_pred[[3]][[lc]] 
    gr2 <- lg_keyr[[3]][[lc]] 
    gr3 <- lg_hist[[3]][[lc]] 
    #grid.arrange(gr1, gr2, gr3, ncol=3)
    #!
    pdf("test.pdf", onefile=FALSE)
    ag <- gridExtra::arrangeGrob(grobs=list(gr1,gr2,gr3),ncol=1,top=nmlcl)
    plotname <- paste0(substr(nmlcl,1,nchar(nmlcl)),".png")
    plotfile <- paste0(pr_path,'/inst/script/voterroll/recorded/ohio/',plotname)
    ggsave(file=plotfile,ag)
    #list(plot=g)
  }
})
#' @export Voterrollreport
Voterrollreport <- setRefClass("Voterrollreport", contains = c('Voterdatabase'))
Voterrollreport$methods(htmlreport=function(reportn='ohio'){

  #! change this
  file_rep_cou <- paste0(pr_path,'/inst/script/voterroll/recorded/ohio/',reportn,'report_cou.html')
  file_rep_sta <- paste0(pr_path,'/inst/script/voterroll/recorded/ohio/',reportn,'report_sta.html')

  v1 <-  predictsc[[2]] %>% kableExtra::kbl() %>%
    kableExtra::kable_paper(full_width = T) %>%
    kableExtra::save_kable(file=file_rep_cou)
  v2 <- predictsc[[3]] %>% kableExtra::kbl() %>%
    kableExtra::kable_paper(full_width = T) %>%
    kableExtra::save_kable(file=file_rep_sta)
})

