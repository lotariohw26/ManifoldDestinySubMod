#' @export Rall
Rall <- function(sel=c(1,2,3)){
  Rxy <- function(rad) {
    matrix(c(cos(rad), sin(rad), 0,
             -sin(rad), cos(rad), 0,
             0, 0, 1), ncol = 3)
  }
  Rxz <- function(rad) {
    matrix(c(cos(rad), 0, sin(rad),
             0, 1, 0,
            -sin(rad), 0, cos(rad)), ncol = 3)
  }
  Ryx <- function(rad) {
    matrix(c(cos(rad), sin(rad), 0,
             -sin(rad), cos(rad), 0,
             0, 0, 1), ncol = 3)
  }
  Ryz <- function(rad) {
    matrix(c(1, 0, 0,
             0, cos(rad), sin(rad),
             0, -sin(rad), cos(rad)), ncol = 3)
  }
  Rzx <- function(rad) {
    matrix(c(cos(rad), 0, sin(rad),
             0, 1, 0,
            -sin(rad), 0, cos(rad)), ncol = 3)
  }
  Rzy <- function(rad) {
    matrix(c(1, 0, 0,
             0, cos(rad), sin(rad),
             0, -sin(rad), cos(rad)), ncol = 3)
  }
    allrot <- list(Rxy,Rxz,Ryx,Ryz,Rzx,Rzy)[sel]
}

#' @export erotation
erotation <-function(
		     dfe=NULL,
		     selvar=NULL,
                     rpar=c(theta=0.2509451852,phi=0.8685213889,rho=0.2020759661),
		     rs=c(1,4,2),
                     mvec=NULL,
		     slice=20
		     ){	

  if (is.null(mvec)) 'ho' else 'abc'
  Ralv <- Rall(sel=rs)
  rofc <<- dfe %>%
    dplyr::select(P,all_of(selvar)) %>%
    dplyr::arrange(P) %>%
    # Standardize variable names
    dplyr::mutate(ui=.[[2]]) %>%
    dplyr::mutate(vi=.[[3]]) %>%
    dplyr::mutate(wi=.[[4]]) %>%
    dplyr::mutate(m1=cos(rpar[1]),m2=cos(rpar[2]),m3=cos(rpar[3])) %>%
    dplyr::mutate(n1=sin(rpar[1]),n2=sin(rpar[2]),n3=sin(rpar[3])) %>%
    # Abc
    dplyr::mutate(st1=rs[1]) %>%
    dplyr::mutate(st2=rs[2]) %>%
    dplyr::mutate(st3=rs[3]) %>%
    # Euler-rotation
    dplyr::mutate(mu=if (is.null(mvec)) mean(ui) else mvec[1]) %>%
    dplyr::mutate(mv=if (is.null(mvec)) mean(vi) else mvec[2]) %>%
    dplyr::mutate(mw=if (is.null(mvec)) mean(wi) else mvec[3]) %>%
    dplyr::mutate(u0=ui-mu) %>%
    dplyr::mutate(v0=vi-mv) %>%
    dplyr::mutate(w0=wi-mw) %>%
    ##
    dplyr::mutate(u1=Ralv[[1]](rpar[1])[1,1]*u0+Ralv[[1]](rpar[1])[1,2]*v0+Ralv[[1]](rpar[2])[1,3]*w0) %>%
    dplyr::mutate(v1=Ralv[[1]](rpar[1])[2,1]*u0+Ralv[[1]](rpar[1])[2,2]*v0+Ralv[[1]](rpar[2])[2,3]*w0) %>%
    dplyr::mutate(w1=Ralv[[1]](rpar[1])[3,1]*u0+Ralv[[1]](rpar[1])[3,2]*v0+Ralv[[1]](rpar[2])[3,3]*w0) %>%
    ##
    dplyr::mutate(u2=Ralv[[2]](rpar[2])[1,1]*u1+Ralv[[2]](rpar[2])[1,2]*v1+Ralv[[2]](rpar[2])[1,3]*w1) %>%
    dplyr::mutate(v2=Ralv[[2]](rpar[2])[2,1]*u1+Ralv[[2]](rpar[2])[2,2]*v1+Ralv[[2]](rpar[2])[2,3]*w1) %>%
    dplyr::mutate(w2=Ralv[[2]](rpar[2])[3,1]*u1+Ralv[[2]](rpar[2])[3,2]*v1+Ralv[[2]](rpar[2])[3,3]*w1) %>%
    ##
    dplyr::mutate(x=Ralv[[3]](rpar[3])[1,1]*u2+Ralv[[3]](rpar[3])[1,2]*v2+Ralv[[3]](rpar[3])[1,3]*w2) %>%
    dplyr::mutate(y=Ralv[[3]](rpar[3])[2,1]*u2+Ralv[[3]](rpar[3])[2,2]*v2+Ralv[[3]](rpar[3])[2,3]*w2) %>%
    dplyr::mutate(z=Ralv[[3]](rpar[3])[3,1]*u2+Ralv[[3]](rpar[3])[3,2]*v2+Ralv[[3]](rpar[3])[3,3]*w2) %>%
    dplyr::mutate(slide=floor(z*50))
}

#' @export me
manobj <- function(enfl=NULL,dfa=NULL,svar='y'){
  polyc <- setNames(as.vector(lapply(enfl, as.character)),LETTERS[1:5])
  la_e <- unlist(polyc[c(LETTERS[1:5])])
  pnr <- sum(la_e!="0") 
  rootdf <- dfa  %>%
    dplyr::mutate(A=pareq(la_e[1],c(as.list(.[,])))) %>%
    dplyr::mutate(B=pareq(la_e[2],c(as.list(.[,])))) %>%
    dplyr::mutate(C=pareq(la_e[3],c(as.list(.[,])))) %>% 
    dplyr::mutate(D=pareq(la_e[3],c(as.list(.[,])))) %>% 
    dplyr::mutate(E=pareq(la_e[3],c(as.list(.[,])))) %>%
    dplyr::group_by(P) %>%
    dplyr::mutate(polsolv=py_polysolver(pnr-1,c(A,B,C,D,E)[1:pnr])) %>%
    dplyr::mutate(!!paste0(svar):=Re(polsolv[1])) %>%
    dplyr::ungroup() 
  rootdf[[svar]]
}
#' @export ballcount
ballcount <- function(ballotsdf=NULL,se=se){
  # Assigning model equations
  sdfc <<- ballotsdf %>% 
    #dplyr::select(P,all_of(selvar))  
    dplyr::mutate(Z=S+T+U+V) %>%
    dplyr::mutate(O=R-Z) %>%
    dplyr::mutate(x=pareq(se[['x_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(y=pareq(se[['y_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(g=pareq(se[['g_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(h=pareq(se[['h_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(m=pareq(se[['m_o']][1],as.list(.[,]))) %>%
    dplyr::mutate(n=pareq(se[['n_o']][1],as.list(.[,]))) %>%
    dplyr::mutate(alpha=pareq(se[['alpha_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(zeta=pareq(se[['zeta_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(lamda=pareq(se[['lamda_s']][1],as.list(.[,]))) %>%
    dplyr::mutate(Omega=pareq(se[['Omega_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(Gamma=pareq(se[['Gamma_h']][1],as.list(.[,]))) %>%
    dplyr::mutate(xi=pareq(se[['xi_o']][1],as.list(.[,]))) 
    #!%>% na.omit()  
}
#' @export pareq
pareq <- function(ste='(x + y*zeta)/(zeta + 1)',lv=list(x=0.75,y=0.25,zeta=1)){
	eval(parse(text=ste),lv)
}
#' @export vpareq
vpareq <- function(dfr=NULL,enf=NULL,ste=NULL)({
  dfr %>% dplyr::mutate(!!enf:=pareq(ste=ste,lv=as.list(.[,]))) %>% dplyr::select(any_of(enf)) %>% as.vector()
})
#' @export plotlymat3d
plotlymat3d <- function(gdf){
    mrdfc <- as.matrix(gdf)
    z <- mrdfc[,1]
    x <- mrdfc[,2]
    y <- mrdfc[,3]
    plotly::plot_ly(x=x,y=y,z=z,type="scatter3d", mode="markers") %>%
      plotly::layout(
		     title =paste0('R2 = ',round(summary(lm(data=gdf))$r.squared,4)), 
		     scene = 
      list(xaxis = list(title = names(gdf)[1]),
	   text='abc',
      yaxis  = list(title = names(gdf)[2]),
      zaxis  = list(title = names(gdf)[3]))) 
}
############################################################################################################################################################
#' @export Countingprocess
Countingprocess <- setRefClass("Countingprocess", 
			       fields=list(sdfc='data.frame',
					   rdfci='data.frame',
					   rdfc='data.frame',
					   rofc='data.frame',
                                           rdfce='data.frame', 
					   quintile='data.frame',
					   desms='data.frame', 
					   r2list='list', 
					   predet='list',
					   sumreg='vector', 
					   psel='vector', 
					   polyc='list',
					   radpar='vector',
					   parameters='list', 
					   preend='list', 
					   parampre='data.frame', 
                                           rotplotly='list',
					   se='list',
					   lx='list',
					   pl_2dsort='list',
					   pl_corrxy='list',
					   pl_rescro='list',
					   pl_3d_mani='list',  
					   all_pl_3d_mani='list',
					   enf='list',
					   mansysl='list',
					   gensysl='list',
					   exnrs='vector',
					   allvar='list',
					   eqpar='list',
					   loss_df='data.frame'
					   ))
Countingprocess$methods(initialize=function(sdfinp=NULL,
					   selvar=c('P','R','S','T','U','V'), 
					   polyn=9, 
					   sortby=alpha
					   ){

  pr_path <- system.file(package='ManifoldDestiny')
  eqpar <<- ManifoldDestiny::eqpar
  stickers <- ManifoldDestiny::stickers
  # Assigning parameters 
  parameters <<- stickers[['parameters']]
  se <<- eqpar$meqs
  lx <<- eqpar$meql
  ils <- c('S','T','U','V')
  sdfc <<- ballcount(dplyr::select(sdfinp,all_of(selvar)),se=se)
  rdfci <<- rdfc <<- sdfc %>% 
    dplyr::arrange(alpha) %>%
    dplyr::mutate(pri=dplyr::row_number()/length(P)) %>%
    dplyr::relocate(pri,.before=P) %>%
    dplyr::relocate(Z,.after=O) 
    #dplyr::arrange(P)

  ## Polynom
  pnset <- min(length(rdfci$pri)-1,polyn)
  ### Init values standard form
  polyc[[1]] <<- lm(rdfci$alpha ~ poly(rdfci$pri, pnset, raw=TRUE))
  ### Init values hybrid form
  polyc[[2]] <<- lm(rdfci$alpha ~ poly(rdfci$pri, pnset, raw=TRUE))
  ##### Init values opposition form
  polyc[[3]] <<- lm(rdfci$lamda ~ poly(rdfci$pri, pnset, raw=TRUE))

})
Countingprocess$methods(r2siminput=function(form=1,latest=0)
{
  rdf <- list(rdfci,rdfc)[[ifelse(latest==0,1,2)]]
  pm <- parameters[[form]]
  regs <- c(mean(rdf$R),sd(rdf$R))
  turn <- c(mean(rdf$V/rdf$R),sd(rdf$V/rdf$R))
  minmax <- c(min(rdf$R),max(rdf$R))
  sv <- c(mean(rdf[[pm[[1]]]]),sd(rdf[[pm[[1]]]]))
  dsv <- c(mean(rdf[[pm[[2]]]])-mean(rdf[[pm[[1]]]]),sd(rdf[[pm[[2]]]]-rdf[[pm[[1]]]]))
  Perc <- list(s=c(mean(rdf$Omega),sd(rdf$Omega)),h=c(mean(rdf$Omega),sd(rdf$Omega)),o=c(mean(rdf$xi),sd(rdf$xi)))
  nprec <- length(rdf$P)
  r2list <<- list(form=form,turn=turn,regs=regs,minmax=minmax,s=sv,ds=dsv,Perc=Perc[[form]],nprec=nprec)
})
Countingprocess$methods(descriptive=function(form=1){
  flp <- c(unname(unlist(ManifoldDestiny::stickers[['parameters']])))
  co <- c('S','T','U','V','R','Z')
  sdv <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(co)),mean))
  mdv <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(flp)),mean))
  sta <- as.data.frame(sapply(dplyr::select(rdfc,dplyr::all_of(c(co,flp))),sd))
  desms <<- data.frame(variable=rownames(sta),mean=c(sdv[,1],mdv[,1]),std=sta[,1])
})

Countingprocess$methods(rotation=function(selvar=c('x','y','alpha'),
				     rpar=c(theta=0,phi=0,rho=0),
				     rs=c(0,0,0),
				     mmeanv=NULL,
			             sli=NULL)
				     {
  dfe <- erotation(dfe=rdfc,selvar=selvar,rpar=rpar,rs=rs,mvec=mmeanv,slice=20)
  rdfc <<- dfe

})



Countingprocess$methods(plext=function(){
  #! automized
  rdfce <<- rdfc %>% 
    dplyr::mutate(alpha2=alpha*alpha,alpha3=alpha*alpha*alpha) %>%
    dplyr::mutate(alphah=alpha*h,alphah2=alpha*h*h,alpha2h=alpha*alpha*h) %>%
    dplyr::mutate(h2=h*h,gh=g*h,g2h=g*g*h,g2=g*g,g3=g*g*g,h3=h*h*h,h2g=h*h*g,hg2=h*g*g) %>%
    dplyr::mutate(gh2=g*h*h) %>%
    dplyr::mutate(g4=g*g*g*g) %>%
    dplyr::mutate(g3h=g*g*g*h) %>%
    dplyr::mutate(g2h2=g*g*h*h) %>%
    dplyr::mutate(g2h2=g*g*h*h) %>%
    dplyr::mutate(gh3=g*h*h*h) %>%
    dplyr::mutate(g4h=g*g*g*g*h) %>%
    dplyr::mutate(h4=h*h*h*h) %>%
    dplyr::mutate(xy=x*y)
})

Countingprocess$methods(purging=function(mdprg=NULL,pri=0){
  rdfv <- rdfci %>% 
    dplyr::arrange(P) %>%
    # Filter
    ## Number of ballots
    dplyr::filter(S>=mdprg$prg$stuv[1]) %>%
    dplyr::filter(T>=mdprg$prg$stuv[2]) %>%
    dplyr::filter(U>=mdprg$prg$stuv[3]) %>%
    dplyr::filter(V>=mdprg$prg$stuv[4]) %>%
    ## Percentages
    dplyr::filter(if_all(c(alpha,x,y,g,h,m,n),~.>mdprg$prg$blup[1]&.<mdprg$prg$blup[2]))
    # Fit filter
    erdfv <- Estimation(rdfv) 
    erdfv$regression(mdprg$sgs$eq)   
    rdfc <<- erdfv$predict_df %>% 
            dplyr::mutate(pre_rnk=row_number(desc(deva))) %>% 
            dplyr::arrange(pre_rnk) %>% 
            #dplyr::filter(pre_rnk>regr[[2]]) %>% dplyr::filter(!P%in%pref) %>% 
            dplyr::mutate(pri=row_number()/length(P)) %>%
            dplyr::arrange(P)
    #!Discarded
  if (pri==1) {print(dim(rdfci)); print(dim(rdfv)); print(dim(rdfc))}
})
#})
Countingprocess$methods(sortpre=function(form=1,
					 polyn=6,
					 sortby='alpha'
					 ){
  frmsel <- list(c(1,2,4,10,3),c(6,7,9,5,8),c(11,12,14,4,13),c(1,2,4,11,12,15))[[form]]
  selvar <- unname(unlist(parameters))[frmsel]
  psel <<- selvar[1:ifelse(form %in% 1:3,5,6)]
  proppar <- rev(selvar)[1]
  srdfc <- rdfc %>%
    dplyr::select(P,all_of(selvar)) %>% 
    dplyr::arrange(alpha) %>%
    dplyr::mutate(pri=row_number()/length(P)) %>%
    dplyr::mutate(!!paste0(proppar,'_m'):=mean(!!rlang::sym(proppar))) %>%
    dplyr::mutate(!!paste0(proppar,'_mr'):=!!rlang::sym(proppar)-!!rlang::sym(paste0(proppar,'_m'))) 
    psel %>% purrr::map(function(x,df=srdfc,p=polyn){
        pred <- c(predict(lm(df[[x]] ~ stats::poly(df$pri,p, raw=TRUE))))
        res <- pred - df[[x]]
        data.frame(pred,res) %>% `colnames<-` (c(paste0(x,'_pred'),paste0(x,'_res')))
    }) %>% as.data.frame(.) -> predictor
  quintile <<- dplyr::bind_cols(srdfc, predictor)

  ## Comments needed
  #plso <- round(polynom::polynomial(unname(coef(polyc[[form]]))),3)
  #pintv <- polynom::integral(polynom::polynomial(plso),c(0,1))
  #plr2 <- round(cor(quintile[[paste0(sortby,'_pred')]],quintile[[sortby]])^2,4)
  #sumreg <<- list(poleq=paste0(plso),polint=pintv,R2=paste0(plr2))
})
Countingprocess$methods(mansys=function(sygen=NULL){
  mansysl <<- sygen     
  sho <- c("_s","_h","_o")[[mansysl$frm]]
  allvar <<- list(pre=mansysl$pre,end=mansysl$end)
  exnrs <<- gsub('v',mansysl$pre[2], gsub('u',mansysl$pre[3],ManifoldDestiny::formpolv[mansysl$me[['plnr']]]))
  enf[[1]] <<- unname(stats::predict(polyc[[mansysl$frm]]))
  enf[[2]] <<- eqpar$meqs[[paste0(mansysl$pre[2],sho)]]
  enf[[3]] <<- py_genpolycoeff(exnrs[[1]],mansysl$pre[[1]],mansysl$pre[[3]])[[1]]
})
Countingprocess$methods(setres=function(czset=NULL,prnt=0){
  frp <- mansysl$frm
  if (!is.null(czset)) polyc[[frp]][[1]][[1]] <<- czset
  if (prnt==1) {
    vec <- unname(polyc[[frp]][[1]])
    print(polynom::integral(polynom::polynomial(vec),c(0,1)))
  }
})
Countingprocess$methods(manimp=function(init_par=NULL,man=TRUE,wn=c(0,0)){
  ## Variables
  allvec <- c(unlist(allvar$pre),unlist(allvar$end))
  sho <- c("_s","_h","_o")[[mansysl$frm]]
  altvec <- paste0(as.vector(unlist(allvar)),sho)
  endp <- paste0(allvec,sho)[c(4,5)]
  pre1 <- enf[[1]] 
  pre2 <- enf[[2]]
  pre3 <- enf[[3]]
  end1 <- se[[endp[1]]][2]
  end2 <- se[[endp[2]]][2]
  lstr   <- paste0("(",allvec[4],"-",altvec[4],")^2")
  lof <- function(kvec=NULL){
    loss_df <<- rdfci %>%
      dplyr::select(P,S,T,U,V,R,Z,all_of(allvec)) %>%
      #dplyr::select(pri,P,S,T,U,V,R,Z,all_of(allvec)) %>%
      data.table::setnames(allvec,altvec) %>%
      ### Parameters
      dplyr::mutate(!!!kvec) %>%
      ### Presetting the first variables
      dplyr::mutate(!!allvec[1]:=enf[[1]]) %>%
      ### Presetting second variable
      dplyr::mutate(!!allvec[2]:=pareq(pre2,c(as.list(.[,])))) %>%
      ### Presetting the Manifold object
      dplyr::mutate(!!allvec[3]:=manobj(enfl=pre3,.[,],allvec[3])) %>%
      ### Adding some noise
      dplyr::mutate(!!allvec[3]:=!!rlang::sym(allvec[3])*(1+rnorm(n(),wn[1],wn[2]))) %>%
      ### Backsolving for the two remaining parameter
      dplyr::mutate(!!allvec[4]:=pareq(end1,c(as.list(.[,])))) %>%
      dplyr::mutate(!!allvec[5]:=pareq(end2,c(as.list(.[,])))) %>%
      dplyr::mutate(LSV:=pareq(lstr,c(as.list(.[,])))) %>%
      #### Backsolving for ballots
      data.table::setnames(c("S","T","U","V"),c("S_o","T_o","U_o","V_o")) %>%
      dplyr::mutate(S=floor(pareq(se[[paste0('S',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(T=floor(pareq(se[[paste0('T',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(U=floor(pareq(se[[paste0('U',sho)]][2],as.list(.[]))))  %>%
      dplyr::mutate(V=floor(pareq(se[[paste0('V',sho)]][2],as.list(.[]))))  %>%
      dplyr::rename(Z_o=Z) %>%
      dplyr::mutate(Z=S+T+U+V)
      ## Loss value
  }
  lv <- function(param=NULL){
    lofdf <- lof(kvec=param)
    nrv <- sum(dplyr::select(lofdf, S, T, U, V) < 0)
    clvl <- sum(lofdf$LSV)+ifelse(nrv>0,nrv*sum(loss_df$LSV),0)
  }
  if (man) {
    man_lores <- lv(param=init_par)
  } else { 
    opt_lores <- optim(par = init_par, fn = lv, method='L-BFGS-B',lower=c(k0=0,k1=0,k2=0),upper=c(k0=0,k1=0,k2=0))
  }
  rdfc <<- dplyr::select(loss_df,P,R,S,T,U,V) %>% ballcount(se=se)
})
#' @export Countinggraphs
Countinggraphs <- setRefClass("Countinggraphs", contains = c('Countingprocess'))
Countinggraphs$methods(plot2d=function(form=1,
    				       labs=list(title=NULL,x="precinct (normalized)",y="percentage",caption=NULL,
				       alpha=1,size=1)
				       ){
  longdf <- tidyr::pivot_longer(quintile,all_of(c(psel,paste0(psel,'_pred'))))
psel
  go <- ggplot2::ggplot(data=longdf) +
    ggplot2::geom_line(data=filter(longdf,name%in%paste0(psel,'_pred')),aes(x=pri,y=value, color=name)) +
    ggplot2::geom_point(data=filter(longdf,name%in%psel),aes(x=pri,y=value, color=name),size=labs$size,alpha=labs$alpha) + 
    ggplot2::labs(title=labs$title,x=labs$x,y=labs$y,caption=labs$caption) +
    ggplot2::ylim(0,1) +
    ggplot2::theme_bw()
    pl_2dsort <<- list(go)
})
Countinggraphs$methods(plotxy=function(form=1,Pexc=NULL){
  dfg <- dplyr::select(rdfc,P,parameters[[form]]) %>% dplyr::filter(!P%in%Pexc) %>% dplyr::select(-P)
  cmb <- combinat::combn(5, 2)
  pl_corrxy <<- lapply(seq(1,dim(cmb)[2]), function(x){
    dfn <- names(dfg[cmb[,x]])
    lim <- apply(dfg,2,max)
    xl <- max(lim[[1]],1)
    yl <- max(lim[[2]],1)
    ggplot2::ggplot(data = dfg, aes(x = !!as.name(dfn[1]), y = !!as.name(dfn[2]))) + geom_point() +
    ggplot2::geom_smooth(method=lm,se=F,show.legend = F) +
    xlim(0, xl) +
    ylim(0, yl) +
    {if(xl==1&&yl==1) geom_abline(slope = 1, intercept = 0) } +
    ggplot2::theme_bw()
  }) 
})
Countinggraphs$methods(resplot=function(form=1){

  selvar <- c(paste0(parameters[[form]][c(1,2,4)],'_res'),paste0(parameters[[form]][c(3)],c("","_m","_mr")))
  dfg <- dplyr::select(quintile,all_of(selvar))
  cmb <- combinat::combn(3, 2)
  pl_rescro <<- lapply(seq(1,dim(cmb)[2]), function(x){
    ggplot2::ggplot(data=dfg,aes(x=selvar[3],y=selvar[3+x])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method=lm,se=F,show.legend = F) +
    ggplot2::labs(x='x',y='y',title="") 
    })
})
Countinggraphs$methods(plotly3d=function(
					 partition=1,
					 sel=list(1:5,6:10),
					 selid=1
					 ){

  rdfcpar <- rdfc %>% dplyr::select(parameters[[partition]][c(4,5,1,2,3)])
  mrdfc <- as.matrix(rdfcpar)
  combi <- combinat::combn(5, 3)
  seq(1,dim(combi)[2]) %>% purrr::map(function(x,comb=combi,df=rdfcpar){
    gdf <- df %>% dplyr::select(combi[,x])
    mrdfc <- as.matrix(gdf)
    z <- mrdfc[,1]
    x <- mrdfc[,2]
    y <- mrdfc[,3]
    plotly::plot_ly(x = x, y = y, z = z, type = "scatter3d", mode = "markers", marker = list(size = 3)) %>%
      plotly::layout(
		     title =paste0('R2 = ',round(summary(lm(data=gdf))$r.squared,4)), 
		     scene = 
      list(xaxis = list(title = names(gdf)[1]),
	   text='abc',
      yaxis  = list(title = names(gdf)[2]),
      zaxis  = list(title = names(gdf)[3]))) 
  }) ->> pl_3d_mani

})
Countinggraphs$methods(rotgraph=function(){
  u0 <- rofc$u0
  v0 <- rofc$v0
  w0 <- rofc$w0
  u1 <- rofc$u1
  v1 <- rofc$v1
  w1 <- rofc$w1
  u2 <- rofc$u2
  v2 <- rofc$v2
  w2 <- rofc$w2
  u3 <- rofc$u3
  v3 <- rofc$v3
  w3 <- rofc$w3
  # Creating the 3D scatter plot
  rotplotly <<- list(plot_ly(type = "scatter3d", mode = "markers", marker = list(size = 3)) %>%
    add_trace(
      x = u0,
      y = v0,
      z = w0,
      mode = "markers",
      type = "scatter3d",
      marker = list(color = "green")
   ) %>%
    add_trace(
      x = u1,
      y = v1,
      z = w1,
      mode = "markers",
      type = "scatter3d",
      marker = list(color = "blue")
    ) %>%
    add_trace(
      x = u2,
      y = v2,
      z = w2,
      mode = "markers",
      type = "scatter3d",
      marker = list(color = "yellow")
    ) %>%
    add_trace(
      x = u3,
      y = v3,
      z = w3,
      mode = "markers",
      type = "scatter3d",
      marker = list(color = "red")
    ) %>%
    layout(scene = list(aspectmode = "cube")))
})
Countinggraphs$methods(gridarrange=function(pl3d=list(selo=1,selm=list(1:5,6:10))){

  ohtml <- div(class="row", style = "display: flex; flex-wrap: wrap; justify-content: center",
  	 div(pl_3d_mani[pl3d$selm[[1]]],class="column"),
  	 div(pl_3d_mani[pl3d$selm[[2]]],class="column"))

  all_pl_3d_mani <<- list(page=htmltools::browsable(ohtml),ohtml=ohtml,one3d=pl_3d_mani,plot2d=pl_2dsort,plotxy=pl_corrxy,plotres=pl_rescro,r2list=r2list,sr=sumreg,abc=rotplotly)
})



