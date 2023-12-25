#######################################################################################
# Libraries
library(googlesheets4)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(htmltools)
library(broom)
library(huxtable)
library(dplyr)
library(htmltools)
library(gridExtra)
library("googlesheets4")
abs_p <- rprojroot::find_rstudio_root_file()
source(paste0(abs_p,'/R/voterrollanalysis.R'))
source(paste0(abs_p,'/R/countingprocess.R'))
source(paste0(abs_p,'/R/regressionanalysis.R'))
source(paste0(abs_p,'/R/misc.R'))
options(scipen=999)
set.seed(1)
#######################################################################################
### Formulas ###
set_n <- c('k0+k1*y','k0+k1*x+k2*y','k0+k1*x+k2*y+k3*zeta')
set_h <- c('k0+k1*g','k0+k1*g+k2*h','k0+k1*g+k2*h+k3*zeta')
set_o <- c('k0+k1*n','k0+k1*n+k2*m','k0+k1*n+k2*m+k3*zeta')
#######################################################################################
# Explicit
# Synthesize data.
# (1) the independent variable `w`.
w.max <- 5 # Max extent of the independent values
w <- expand.grid(seq(-w.max,w.max), seq(-w.max,w.max))
w <- complex(real=w[[1]], imaginary=w[[2]])
w <- w[Mod(w) <= w.max]
n <- length(w)
# (2) the dependent variable `z`.
beta <- c(-20+5i, complex(argument=2*pi/3, modulus=3/2))
sigma <- 2; rho <- 0.8 # Parameters of the error distribution
library(MASS) #mvrnorm
set.seed(17)
e <- mvrnorm(n, c(0,0), matrix(c(1,rho,rho,1)*sigma^2, 2))
e <- complex(real=e[,1], imaginary=e[,2])
z <- as.vector((X <- cbind(rep(1,n), w)) %*% beta + e)
# Fit the models.
print(beta, digits=3)
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
print(beta.r <- coef(lm(Re(z) ~ Re(w) + Im(w))), digits=3)
print(beta.i <- coef(lm(Im(z) ~ Re(w) + Im(w))), digits=3)
#######################################################################################
break1 = function(X) {
        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
}
break3 = function(X) {
        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
}
fit.complex = function(Y, X.List) {
	browser()

	abc <- do.call(data.frame,lapply(X.List, function(x){
		      list(c(Re(x),rep(0,length(x))),c(rep(0,length(x)),Im(x))) 
	}))
	names(abc) <- c("x0","x0i","x1","x1i")

	Data <- data.frame(ind=seq(1,length(Y)),part=rep(c('r','i'),length(Y)),YF=break1(Y)) %>%
		dplyr::arrange(desc(part),ind) 
		dplyr::bind_cols(abc) %>%
		dplyr::arrange(ind)
        
	Model = lm(as.formula('YF~x0+x0i+x1+x1i+x2+x2i-1'), data=Data)


        ## Formula + Model
        Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
        Model = lm(as.formula(Formula), data=Data)
        ## Make them complex again
        Coeffs = sapply(seq_along(X.List),
                function(N) {
			# N <- 2
                        ( Model$coefficients[[ X.Names[[2*N-1]] ]]
                        + Model$coefficients[[ X.Names[[2*N]] ]]*1i )
                })
        names(Coeffs) = names(X.List)
        Model$coefficients.complex = Coeffs
        Model
}
### Test 1
abc <- fit.complex(z,X)
### Complex
gs_complex <- 'https://docs.google.com/spreadsheets/d/1x1w2hv8JdAA5Zq5C-SbmsSBTA1fhO1aUuDyFRgEeHK0/edit#gid=263283852'
df_complex <- googlesheets4::read_sheet(gs_complex, sheet=1, range="A4:F1234")
names(df_complex) <- c('Psi_apc','zeta','h','Psi_c','alpha','Psi_b')
npr <- dim(df_complex)[1]
Y <- (df_complex$zeta+1i*df_complex$Psi_apc)
X <- list(X0=rep(c(1+1i*1),npr),X1=df_complex$h+1i*df_complex$zeta,X2=df_complex$alpha+1i*df_complex$Psi_b) 
Model = fit.complex(Y=Y,X.List=X)
#######################################################################################
break1 = function(X) {
        do.call(c, lapply(X, function(x) { c(Re(x), Im(x)) }))
}

break2 = function(X) {
        do.call(c, lapply(X, function(x) { c(-Im(x), Re(x)) }))
}
fit.complex = function(Y, X.List) {
browser()
        # Split into real variables
        YF = break1(Y)
Y
        XF.List = do.call(c, lapply(X.List,
                function(x) { list(break1(x), break2(x)) } ))

        # Make the data.frame
        Data = data.frame(Y = YF)
        X.Names = paste('X', 1:length(XF.List), sep='')

        for (N in seq_along(XF.List)) {
                Data[[ X.Names[[N]] ]] = XF.List[[N]]
		print(Data)
        }
	View(XF.List)

        # Formula + Model
        Formula = paste("Y ~ ", paste(X.Names, collapse='+'), "-1")
	View(Data)
        Model = lm(as.formula(Formula), data=Data)

        # Make them complex again
        Coeffs = sapply(seq_along(X.List),
                function(N) {
                        ( Model$coefficients[[ X.Names[[2*N-1]] ]]
                        + Model$coefficients[[ X.Names[[2*N]] ]]*1i )
                })
        names(Coeffs) = names(X.List)

        Model$coefficients.complex = Coeffs

        Model
}
Beta0 = 1 + 3i
Beta1 = 3 - 2i

X = runif(15, 0, 10)
Y = (Beta0 + Beta1*X +
        rnorm(length(X), 0, 0.7) * exp(1i*runif(length(X), 0, 2*pi))
)
Y
X
Model = fit.complex(Y, list(
         const = 0*X+1,
        linear = X
))
Beta0.Est = Model$coefficients.complex[[1]]
Beta1.Est = Model$coefficients.complex[[2]]
> Beta0.Est
[1] 1.090385+3.017922i
> Beta1.Est
[1] 2.912617-2.030427i
#######################################################################################
# Explicit
# Synthesize data.
# (1) the independent variable `w`.
w.max <- 5 # Max extent of the independent values
w <- expand.grid(seq(-w.max,w.max), seq(-w.max,w.max))
w <- complex(real=w[[1]], imaginary=w[[2]])
w <- w[Mod(w) <= w.max]
n <- length(w)
# (2) the dependent variable `z`.
beta <- c(-20+5i, complex(argument=2*pi/3, modulus=3/2))
sigma <- 2; rho <- 0.8 # Parameters of the error distribution
library(MASS) #mvrnorm
set.seed(17)
e <- mvrnorm(n, c(0,0), matrix(c(1,rho,rho,1)*sigma^2, 2))
e <- complex(real=e[,1], imaginary=e[,2])
z <- as.vector((X <- cbind(rep(1,n), w)) %*% beta + e)
# Fit the models.
###
zc <- z
Xc <- X
z <- Y
X <- X[,1]
print(beta.hat <- solve(Conj(t(X)) %*% X, Conj(t(X)) %*% z), digits=3)
print(beta.r <- coef(lm(Re(z) ~ Re(w) + Im(w))), digits=3)
print(beta.i <- coef(lm(Im(z) ~ Re(w) + Im(w))), digits=3)
# Show some diagnostics.
#Y
#Beta1 = Model$coefficients.complex[[1]]
#Beta2 = Model$coefficients.complex[[2]]
