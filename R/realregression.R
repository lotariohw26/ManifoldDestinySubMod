#' @export strform 
strform <- function(selv=NULL){ 
  # Shave 1
  cvec <- c("x","y","zeta","g","h","Gamma","n","m","xi","Psi","alpha","u3","v3","w3")
  trc <- cvec[sapply(cvec,grepl,selv)]
  shave1 <- selv[[1]]
  # Shave 2
  nk <- stringr::str_count(shave1,"k")
  subk <- paste0(" \\+ k0|","k0|",paste0("k",each=c(1:nk),"*|",collapse=""),"\\*")
  paste0(names(selv),' ~ ',gsub(subk,"",shave1))
}
#' @export totwomodes
totwomodes <- function(A=NULL,B=NULL,C=NULL,D=NULL,dfi=NULL){
  ou <- dfi %>%
    dplyr::mutate(a=eval(parse(text=A))) %>%
    dplyr::mutate(b=eval(parse(text=B))) %>%
    dplyr::mutate(c=eval(parse(text=C))) %>%
    dplyr::mutate(d=eval(parse(text=D)))
}
############################################################################################################################################################
#' A class description
#' @export Estimation
Estimation <- setRefClass("Estimation", fields=list(
						edfc='data.frame',
						rsedfc='data.frame',
						predict_df='data.frame',
						pred_df_pol='data.frame',
						compare='data.frame',
						fnr='numeric',
						lpy='list',
						frvar='vector',
						regequ='character',
						regsum='list',
						regform='vector',
						resplots='list', 
						rotplotly='list',
						kvec='vector',
						param='vector',
						syequ='list',
						roto='vector',
						comdesc='data.frame',
						radpar='vector',
						lpku='list'
						))
Estimation$methods(initialize=function(rdfcinp=NULL,form=1){
  edfc <<- rdfcinp
  roto <<- ifelse(all(c("ui", "vi", "wi") %in% colnames(edfc)), 1, 0)
  fnr <<- form
  param <<- ManifoldDestiny::stickers[['parameters']][[fnr]]
  syequ <<- ManifoldDestiny::eqpar$meqs
  radpar <<- c(theta=0,phi=0,rho=0)
  lpku <<- list(
    S = list(
      x = c(Sd = 'x*(Z-U-V)', Td = '(1-x)*(Z-U-V)', Ud = 'U', Vd = 'V'),'S~S_hat',
      y = c(Sd = 'S', Td = 'T', Ud = 'y*(Z-S-T)', Vd = '(1-y)*(Z-S-T)','U~U_hat')
    ),
    H = list(
      g = c(Sd = 'g*(Z-T-U)', Td = 'T', Ud = 'U', Vd = '(1-g)*(Z-T-U)','S~S_hat'),
      h = c(Sd = 'S', Td = 'h*(Z-T-U)', Ud = 'h*(Z-T-U)', Vd = 'V','S~S_hat')
    ),
    O = list(
      n = c(Sd = 'm*(Z-T-V)', Td = 'T', Ud = '(1-m)*(Z-T-V)', Vd = 'V','S-S_hat'),
      m = c(Sd = 'S', Td = 'm*(Z-T-V)', Ud = 'U', Vd = '(1-m)*(Z-T-V)','T-T_hat')
    )
  )
})
Estimation$methods(regression=function(regequ=c("alpha=k0+k1*x+k2*y")){
  regform <<- strsplit(regequ, "=")[[1]]
  # Formula
  forms <- gsub("\\*","",paste0(regform[1],"~",gsub("\\*\\*","",gsub("k\\d+","",regform[2]))))
  formo <- as.formula(forms)  
  allv <- all.vars(formo)
  # Extended data table
  un <- intersect(allv,names(edfc))
  lh <- setdiff(allv,un)
  if (length(lh)>0){
    rh <- sapply(seq_along(lh),function(t) gmp(lh[t]))
    edfc <<- mutate(edfc,!!!setNames(lapply(rh, rlang::parse_expr), lh))
  }
  # Predict
  regsum[[1]] <<- lm(formo,data=edfc)
  endh <- paste0(regform[1],"_hat")
  endr <- regform[1]
  kvec <<- broom::tidy(regsum[[1]])$estimate
  predict_df <<- edfc %>%
    dplyr::mutate(!!endh:=stats::predict(regsum[[1]])) %>%
    dplyr::mutate(dev=!!rlang::sym(endr)-!!rlang::sym(endh)) %>%
    dplyr::mutate(deva=abs(dev)) %>%
    dplyr::mutate(pre_rnk=row_number(desc(deva))) 
})
Estimation$methods(diagnostics=function(){
  resplots <<- lapply(1:length(regsum), function(x) {
    ml <- regsum[[x]]
    dfgpl <- data.frame(xind=as.numeric(rownames(ml$model)),res=ml$re,endv=ml$model[[1]],endvp=ml$fitted.values) %>% dplyr::mutate(sres=sort(res))
    lh <- ggplot(dfgpl, aes(x = res)) + geom_histogram(bins = 100, fill = 'steelblue', color = 'black') + labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')+
    theme_bw()
    la <- ggplot(dfgpl, aes(x = endv, y=endvp)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_bw()
    lr <- ggplot(dfgpl, aes(x = xind, y=res )) + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_bw()
    lq <- ggplot(dfgpl, aes(x = xind , y = sres)) + geom_point() + theme_bw()
    sht <- stats::shapiro.test(predict_df$dev)
    list(lh=lh,lr=lr,la=la,lq=lq,sht=sht)
  }) 
})
Estimation$methods(hat_predict=function(svf='y',rnr=1){
  kvec <<- broom::tidy(regsum[[1]])$estimate
  names(kvec) <<- paste0("k", 0:(length(kvec) - 1))
  if (roto==0){
    ex <- gsub("\\^","**",regform[2])
    sd <- regform[1]
    eurv <- c(0,0,0)
    svfi <- c(svf,svf)
    lpy <<- py_genpolycoeff(expr=ex,solvd=sd,solvf=svfi[2],eur=eurv)
    setNames(as.vector(lapply(lpy[[1]], as.character)),LETTERS[1:5])
    pnr <- sum(lpy[[1]]!="0") 
  }
  if (roto==1){
    ex <- gsub("\\^","**",regform[2])
    sd <- regform[1]
    eurv <- c(edfc$st1[1],edfc$st2[2],edfc$st3[3])
    lpy <<- py_genpolycoeff(expr=NULL,solvd=sd,solvf='Z',eur=eurv,dnr=2)
    lpy[[1]] <<- setNames(as.vector(lapply(lpy[[1]],as.character)),LETTERS[1:5])
    lpy[[2]] <<- setNames(as.vector(lapply(lpy[[2]],as.character)),c("x","y","z"))
    lpy[[3]] <<- setNames(as.vector(lapply(lpy[[3]],as.character)),paste0(rep(letters[1:3],each=3),seq(1,3)))
  }
  pred_df_pol <<- predict_df %>% dplyr::arrange(P) %>%
    dplyr::mutate(nr=pnr) %>%
    dplyr::mutate(!!!kvec) %>%
    dplyr::mutate(A=pareq(as.character(lpy[[1]][[1]]),.[,])) %>%
    dplyr::mutate(B=pareq(as.character(lpy[[1]][[2]]),.[,])) %>%
    dplyr::mutate(C=pareq(as.character(lpy[[1]][[3]]),.[,])) %>%
    dplyr::mutate(D=pareq(as.character(lpy[[1]][[4]]),.[,])) %>%
    dplyr::mutate(E=pareq(as.character(lpy[[1]][[5]]),.[,])) %>%
    dplyr::group_by(P) %>%
    dplyr::mutate(polsolv=py_polysolver(pnr-1,c(A,B,C,D,E)[1:pnr])) %>%
    dplyr::mutate(!!paste0(svf[1],'_hat'):=Re(polsolv[1])) %>%
    dplyr::ungroup() 
  regsum[[2]] <<- lm(as.formula(paste0(svf[1],"~", svf[1],'_hat')),data=pred_df_pol)
})
Estimation$methods(hat_intcomp=function(){
  svf <- as.character(summary(regsum[[2]])[[2]][[2]])
  lpkus <- lpku[[fnr]][[svf]]
  BLM <- c('S','T','U','V','Z')
  slvh <- slv <- c(BLM,svf)
  slvh[slvh==svf] <- paste0(svf,'_hat')
  compare <<- dplyr::select(pred_df_pol,all_of(slvh)) %>% data.table::setnames(slv) %>%
  # Backsolving for ballots S,T,U,V
  dplyr::mutate(!!names(lpkus[1]):=pareq(lpkus[1],as.list(.[]))) %>% 
  dplyr::mutate(!!names(lpkus[2]):=pareq(lpkus[2],as.list(.[]))) %>%
  dplyr::mutate(!!names(lpkus[3]):=pareq(lpkus[3],as.list(.[]))) %>%
  dplyr::mutate(!!names(lpkus[4]):=pareq(lpkus[4],as.list(.[]))) %>%
  ## Compare S,T,U,V #!
  dplyr::mutate(S_hat:=ifelse(Sd>S,floor(Sd),ceiling(Sd))) %>%
  dplyr::mutate(T_hat:=ifelse(Td>T,floor(Td),ceiling(Td))) %>%
  dplyr::mutate(U_hat:=ifelse(Ud>U,floor(Ud),ceiling(Ud))) %>%
  dplyr::mutate(V_hat:=ifelse(Vd>V,floor(Vd),ceiling(Vd))) %>%
  ## Diff S,T,U,V
  dplyr::mutate(diff_S=S-S_hat) %>%
  dplyr::mutate(diff_T=T-T_hat) %>%
  dplyr::mutate(diff_U=U-U_hat) %>%
  dplyr::mutate(diff_V=V-V_hat) 
  comps <- paste0('diff_',substr(lpkus[[5]], 1, 1))
  regsum[[3]] <<- lm(as.formula(lpkus[[5]]),data=compare)
  vnd <- c(
    meantotvote=mean(compare$Z),
    nmbpre=length(compare[[comps]] == 0),
    match_0=sum(compare[[comps]] == 0),
    prc=100*sum(compare[[comps]] == 0)/length(compare[[comps]] == 0),
    mean=mean(compare[[comps]]),
    sd=sd(compare[[comps]]),
    max=max(compare[[comps]]),
    match_1=sum(compare[[comps]]==1|compare[[comps]]==-1),
    match_2=sum(compare[[comps]]==2|compare[[comps]]==-2),
    match_3=sum(compare[[comps]]==3|compare[[comps]]==-3),
    prc0123=100*sum(abs(compare[[comps]] - 0) <= 3)/length(compare[[comps]]))
  comdesc <<- data.frame(stats=names(vnd),values=vnd)
})
############################################################################################################################################################
