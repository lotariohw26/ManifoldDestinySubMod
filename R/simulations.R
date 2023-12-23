#' @export ballcastsim
ballcastsim <- function(
	dfm=dfm,
	probw=c(0.5,0), 	
	probva=c(0.7,0.2,0.03,0.00), 				
	probvb=c(0.7,0.2,0.03,0.00), 
	ztech=c(0,0)
			){

  # Simulate prob
  probvrnd <<- dfm %>%
    dplyr::mutate(ZV=rnorm(n(),probw[1],probw[2])) %>%
    dplyr::mutate(N=runif(n(),ztech[1],ztech[2])) %>%
    dplyr::mutate(p3=(1-ztech)*(1-pmax(0, pmin(1,rnorm(n(),probva[1],probva[3]))))) %>%
    dplyr::mutate(p6=(1-ztech)*(1-pmax(0, pmin(1,rnorm(n(),probvb[1],probvb[3]))))) %>%
    dplyr::mutate(p2=(1-p3)*pmax(0, pmin(1,rnorm(n(),probva[2],probva[4])))) %>%
    dplyr::mutate(p5=(1-p6)*pmax(0, pmin(1,rnorm(n(),probvb[2],probvb[4])))) %>%
    dplyr::mutate(p1=1-p2-p3) %>%
    dplyr::mutate(p4=1-p5-p6) %>%
    dplyr::select(P,ZV,N,p1,p2,p3,p4,p5,p6)

  ballcodf <- dfm %>% dplyr::left_join(probvrnd,by='P') %>% base::split(.$P) %>%
       purrr::map(function(x){
  # Assigning voters in each precinct
  cp <- stats::rbinom(x$R,1,x$ZV) 
  ## Setting up vector frame
  dc <- data.frame(P=x$P,ZV=x$ZV,R=x$R,C=cp,p1=x$p1,p2=x$p2,p3=x$p3,p4=x$p4,p5=x$p5,p6=x$p6) %>%
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
}

#' @export r2outgp
r2outgp <- function(dfr2=gg_df){

  # Out 1
  # Define vector of values to highlight
  gg_dfa <- gg_df %>% dplyr::arrange(perc1)
  
  # Out 2
  gg_dfp <- gg_df %>% tidyr::gather(key = 'variable', value = 'value',c(-perc1,-perc2))
  gg_h <- ggplot(gg_dfp, aes(x = value, fill = variable)) +
    geom_histogram(alpha = 0.5, bins = 30) +
    scale_fill_manual(values = c("red", "green", "blue"))

  list(dfa=gg_dfa,ggh=gg_h)
}

#' @export r2simn
r2simn <- function(nprec=300,
		   regs=c(3.15,0.25),
		   minmax=c(400,4000),
		   turn=c(0.5,0.10),
		   Invper=c(0.5,0.10),
		   u=c(0.6,0.10),
		   dv=c(-0.2,0.08),
		   form=1)
		   {
    BL <- list(fm1=c('S','T','U','V'),fm2=c('S','V','T','U'),fm3=c('S','U','T','V'))[[form]]
    form <- list(std=c('alpha~x+y','alpha~x+y+zeta'),
                 std=c('alpha~g+h','alpha~g+h+gamma'),
                 std=c('alpha~n+m','alpha~n+m+xi'))[[form]]
    # Box
    dfb <- data.frame(P=seq(1,nprec)) %>%
    dplyr::mutate(genreg=10^qnorm(runif(n()),regs[1],regs[2])) %>%
    dplyr::mutate(R=round(ifelse(genreg>minmax[2]|genreg<minmax[1],runif(1,minmax[1],minmax[2]),genreg))) %>%
    dplyr::mutate(Z=round(R*qnorm(runif(n()),turn[1],turn[2]))) %>%
    # Form 
    dplyr::mutate(!!paste0('OpT'):=round(Z*rnorm(n(),Invper[1],Invper[2]))) %>%
    dplyr::mutate(!!paste0('TpF'):=Z-OpT) %>% dplyr::mutate(gen_u=rnorm(n(),u[1],u[2])) %>%
    dplyr::mutate(gen_v=rnorm(n(),dv[1],dv[2])+gen_u) %>%
    # Ballots
    dplyr::mutate(!!paste0(BL[1]):=round(gen_u*OpT)) %>%
    dplyr::mutate(!!paste0(BL[2]):=round(OpT-S)) %>%
    dplyr::mutate(!!paste0(BL[3]):=round(gen_v*TpF)) %>%
    dplyr::mutate(!!paste0(BL[4]):=round(TpF-U)) 
    
    dfb2 <- Countingprocess(dfb)$sdfc

    # Prediction
    pe_1 <- stats::predict(lm(as.formula(form[1]),data=dfb2)) 
    pe_2 <- stats::predict(lm(as.formula(form[2]),data=dfb2)) 
    # Comparison
    dfc <- dfb2 %>% 
	#1 Alpha
	dplyr::mutate(alpha_hat_1=pe_1) %>%
	dplyr::mutate(alpha_hat_2=pe_2) %>%
  	dplyr::mutate(TSS_1=(alpha-mean(alpha))^2) %>%
  	dplyr::mutate(RSS_1=(alpha-alpha_hat_1)^2) %>%
  	dplyr::mutate(sTSS_1=sum(TSS_1)) %>%
  	dplyr::mutate(sRSS_1=sum(RSS_1)) %>%
  	dplyr::mutate(R2_1=1-sRSS_1/sTSS_1) %>%
	# Ballots
  	dplyr::mutate(TSS_2=(alpha-mean(alpha))^2) %>%
  	dplyr::mutate(RSS_2=(alpha-alpha_hat_2)^2) %>%
  	dplyr::mutate(sTSS_2=sum(TSS_2)) %>%
  	dplyr::mutate(sRSS_2=sum(RSS_2)) %>%
	# Output
  	dplyr::mutate(R2_2=1-sRSS_2/sTSS_2)

    list(r2reg=unique(dfc$R2_1),receil=unique(dfc$R2_2),dfcopy=dfc)
}

#' @export SimVoterdatabase
SimVoterdatabase <- setRefClass("SimVoterdatabase",fields=
				   list(
					ballcous='data.frame',
					r2dflook='list',
					htmlr2='data.frame',
					ggplr2='list'
				   )
)

SimVoterdatabase$methods(initialize=function(initdf=NULL){
  ballcous <<- Countingprocess(initdf)$sdfc
})
SimVoterdatabase$methods(r2sim=function(rept=10,form=1)
{
    #ManifoldDestiny::stickers[[1]][[form]]
    #$standard
    #[1] "x"     "y"     "zeta"  "alpha" "lamda"
    #$hybrid
    #[1] "g"     "h"     "Gamma" "alpha" "Omega"
    #$opposition
    #[1] "m"     "n"     "xi"    "lamda" "Omega"
    # Fixed
    srs <- list(st=c('Omega','x','y'),hy=c('lamda','h','g'),op=c('Omega','m','n'))[[form]]
    v_nprec <- length(ballcous$P)
    v_regs <- c(mean(ballcous$R)/1000,sd(ballcous$R)/1000) #c(3.15,0.25)
    v_minmax <- range(ballcous$R)
    v_turn <- c(mean(ballcous$R/ballcous$Z),sd(ballcous$R/ballcous$Z)) #c(0.5,0.01) 
    # Form dependent
    v_Invper <- c(mean(ballcous[[srs[1]]]),sd(ballcous[[srs[1]]]))  
    v_u <- c(mean(ballcous[[srs[2]]]),sd(ballcous[[srs[2]]]))
    v_dv <- c(mean(ballcous[[srs[[3]]]]-ballcous[[srs[[2]]]]),sd(ballcous[[srs[[3]]]]-ballcous[[srs[[2]]]]))
    # R2 calculations
    tf <- replicate(rept, r2simn(nprec=v_nprec,regs=v_regs,minmax=v_minmax,turn=v_turn,Invper=v_Invper,u=v_u,dv=v_dv))
    dfgp <- data.frame(r2a=unlist(tf[seq(1,length(tf),3)]),r2b=unlist(tf[seq(2,length(tf),3)])) %>% mutate(perc = ntile(r2a, 100)) 
    # Input DF2
    percentiles <- c(90, 95, 99)
    nstd <- c(1,2,5)
    std <- mean(dfgp$r2a)+nstd*sd(dfgp$r2a)
    perc1 <- quantile(dfgp$r2a,probs = percentiles / 100)
    perc2 <- quantile(dfgp$r2b,probs = percentiles / 100)
    percdf <- data.frame(perc1,perc2,nstd,std) %>% data.table::setnames(c("Perc r2a","Perc r2b","Nstd","Vstd")) 
    r2dflook <<- list(dfgp,percdf)
})
SimVoterdatabase$methods(htmltable=function(){
 
  htmlr2 <<- r2dflook[[1]] #kableExtra::kbl() #%>% kableExtra::kable_paper(full_width = F) 

})
SimVoterdatabase$methods(gghist=function(){
  dfgp <- r2dflook[[1]] %>% tidyr::pivot_longer(cols=c("r2a","r2b")) %>% dplyr::arrange(name,perc)
  percd <- r2dflook[[2]] 
    ggplot(dfgp,aes(x=value, fill=name)) + 
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) + 
    labs(title = "histogram of values by category", x = "value", y = "count") +
    #geom_vline(xintercept = as.numeric(percd[1,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[2,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[3,1]), linetype = "dashed", color = "blue") +
    #geom_vline(xintercept = as.numeric(percd[3,4]), linetype = "solid", color = "red") +
    #geom_label(y=0,x=as.numeric(percd[1,1]),label="*",geom="label") +
    #geom_label(y=0,x=as.numeric(percd[2,1]),label="**",geom="label") +
    #geom_label(y=0,x=as.numeric(percd[3,1]),label="***",geom="label") +
    theme_minimal() +
    scale_fill_manual(values = c("#0072b2", "#e69f00"))  # set fill colors
})

