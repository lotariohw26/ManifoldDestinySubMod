####################################################################################################################
########################################################################################################################
# Cleaning namespace
for name in dir():
  if not name.startswith('_'):
      del globals()[name]
# Import packages
#import json
import sympy
import numpy
import sys
from sympy import solve, Eq, symbols, latex, simplify, diff
import pandas
import os
import rpy2
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr, data
rroot=importr('rprojroot')
abs_path = rroot.find_rstudio_root_file()[0]
csvfile=abs_path+'/inst/script/python/csv/parameters.csv'
readmodvar = pandas.read_csv(csvfile, sep=',') 
sympy.var(readmodvar.iloc[:,0])
beforems = set(dir())
########################################################################################################################
#########################################################################################################################
### Stanvarv form proportions
x_s       = [Eq(x,S/(S+T))]
y_s       = [Eq(y,U/(U+V))]
zeta_s    = [Eq(zeta,(U+V)/(S+T)),Eq(zeta,(x-alpha)/(alpha-y))]
alpha_s   = [Eq(alpha,(S+U)/(S+T+U+V)),Eq(alpha,(x+zeta*y)/(1+zeta))]
lamda_s   = [Eq(lamda,(S+V)/(S+T+U+V)),Eq(lamda,(x+zeta*(1-y))/(zeta+1))]
###########################################################################################################################
######### Hybrid form proportions
g_h       = [Eq(g,S/(S+V))]
h_h       = [Eq(h,U/(T+U))]
Gamma_h   = [Eq(Gamma,(T+U)/(S+V)),Eq(Gamma,(g-alpha)/(alpha-h))]
alpha_h   = [Eq(alpha,(S+U)/(S+T+U+V)),Eq(alpha,(g+Gamma*h)/(1+Gamma))]
Omega_h   = [Eq(Omega,(S+T)/(S+T+U+V)),Eq(Omega,(g+Gamma*(1-g))/(Gamma+1)),Eq(Omega,(2*g+Gamma)/(Gamma+1)-alpha)]
###############################################################################################################################
###### Oppostion form proportions
xi_o      = [Eq(xi,(T+V)/(S+U)),Eq(xi,(m-Omega)/(Omega-n)),Eq(xi,(m-lamda)/(lamda-(1-n)))]
n_o       = [Eq(n,S/(S+U))]
m_o       = [Eq(m,T/(T+V))]
lamda_o   = [Eq(lamda,(S+V)/(S+T+U+V)),Eq(lamda,(2*m+xi)/(xi+1)-Omega)]
Omega_o   = [Eq(Omega,(S+T)/(S+T+U+V)),Eq(Omega,(m+xi*n)/(xi+1))]
#############################################################################################################################
##### Integers
S_s = [Eq(s,S),Eq(s,x*Z*(alpha-y)/(x-y))]
T_s = [Eq(t,T),Eq(t,(1-x)*Z*(alpha-y)/(x-y))]
U_s = [Eq(u,y*(Z-S-T)),Eq(u,y*Z*(alpha-y)/(x-y)*zeta)]
V_s = [Eq(v,y*(Z-S-T)),Eq(v,(1-y)*Z*(alpha-y)/(x-y)*zeta)]
##
S_h = [Eq(s,g*(Z-T-U)),Eq(s,g*Z*(alpha - h)/(g - h))]
T_h = [Eq(t,T),Eq(t,(1-h)*Z*(alpha - h)/(g - h)*Gamma)]
U_h = [Eq(u,U),Eq(u,h*Z*(alpha - h)/(g - h)*Gamma)]
V_h = [Eq(v,(1-g)*(Z-T-U)),Eq(v,(1-g)*Z*(alpha - h)/(g - h))]
##
S_o = [Eq(s,Z*n/(1+xi)),Eq(s,t+u)]
T_o = [Eq(t,(Z*m)/(1+1/xi))]
U_o = [Eq(u,Z*(1-n)/(1+xi))]
V_o = [Eq(v,(Z*(1-m))/(1+1/xi))]
###########################################################################################################################
###### Implications:
### Standard form
#y_s.append(Eq(y,solve(alpha_s[1],y)[0]))
### Hybrid form
#solve(alpha_s[1],y)
### Oppostion form
### Interaction
#############################################################################################################################
### Causality
#az_df1 = diff(solve(alpha_s[1],alpha)[0],zeta)
#az_df2 = diff(diff(solve(alpha_s[1],alpha)[0],zeta),zeta)
#aG_df1 = diff(solve(alpha_h[1],alpha)[0],Gamma)
#aG_df2 = diff(diff(solve(alpha_h[1],alpha)[0],Gamma),Gamma)
#############################################################################################################################
##########################################################################################################################
###### Storing ###
##### Storing ###
afterms = set(dir())
modvarlist = list(afterms - beforems)
modvarlist.remove('beforems')
modvarlist
modeqs = dict()
modeql = dict()
dfs = [] 
dfl = [] 
parv = 'empty'
for i in range(0,len(modvarlist)):
  modeqs[parv] = dfs
  modeql[parv] = dfl
  parv = modvarlist[i]
  dfs = [] 
  dfl = [] 
  nid = len(eval(modvarlist[i])) 
  for j in range(0,nid):
    dfs.append(str(eval(modvarlist[i])[j].rhs))
    dfl.append(latex(eval(modvarlist[i])[j].rhs))
modeqs[parv] = dfs
modeql[parv] = dfl
modeqs.pop('empty')
modeql.pop('empty')
##########################################################################################################################
