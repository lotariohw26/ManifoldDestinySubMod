import os
import sympy
import numpy
import sys
from sympy import solve, Eq, symbols, latex, simplify, diff
import pandas
import os

# Import packages
sympy.init_printing()
beforems = set(dir())
# Defining variables
x_2, alpha_1, lambda_1, omega_1, x_1, y_1, g_1, h_1, m_1, zeta, gamma, xi, omega_2, y_2, lambda_2, g_2, h_2, n_1, alpha_2, m_2, n_2, x_2, alpha_2, h_2, m_2 = symbols("x_2 alpha_1 lambda_1 omega_1 x_1 y_1 g_1 h_1 m_1 zeta gamma xi omega_2 y_2 lambda_2 g_2 h_2 n_1 alpha_2 m_2 n_2 x_2 alpha_2 h_2 m_2")



from sympy import symbols, Rational

# Define symbols
s, t, u, v = symbols('s t u v')

# North and South ratios
x1 = s / (s + t)
y1 = u / (u + v)
x2 = t / (s + t)
y2 = v / (u + v)

# West and East ratios
g1 = s / (s + v)
h1 = u / (u + t)
g2 = v / (s + v)
h2 = t / (u + t)

# Northwest and Northeast ratios
m1 = s / (s + u)
n1 = t / (t + v)
m2 = u / (s + u)
n2 = v / (t + v)

alpha1 = (s + u) / ((s + u) + (t + v))
xi = (t + v) / (s + u)
alpha2 = Rational(1, 1) - alpha1
xi_identity = alpha2 / alpha1

alpha2_inv = (s + u) / ((s + u) + (t + v))
xi_inv = (t + v) / (s + u)
alpha1_inv = Rational(1, 1) - alpha2_inv
xi_inv_identity = alpha1_inv / alpha2_inv

# West Aggregate, East to West Proportion
lambda1 = (s + v) / ((s + v) + (u + t))
gamma = (u + t) / (s + v)
lambda2 = Rational(1, 1) - lambda1
gamma_identity = lambda2 / lambda1

# East Aggregate, West to East Proportion
lambda2_inv = (u + t) / ((s + v) + (u + t))
gamma_inv = (s + v) / (u + t)
lambda1_inv = Rational(1, 1) - lambda2_inv
gamma_inv_identity = lambda1_inv / lambda2_inv

# North Aggregate, South to North Proportion
Omega1 = (s + t) / ((s + t) + (u + v))
zeta = (u + v) / (s + t)
Omega2 = Rational(1, 1) - Omega1
zeta_identity = Omega2 / Omega1

# South Aggregate, North to South Proportion
Omega2_inv = (u + v) / ((s + t) + (u + v))
zeta_inv = (s + t) / (u + v)
Omega1_inv = Rational(1, 1) - Omega2_inv
zeta_inv_identity = Omega1_inv / Omega2_inv
























# Define Equations
l1 = [
    Eq(x_1, alpha_1 + zeta*(alpha_1 - y_1)),
    Eq(g_1, alpha_1 + gamma*(alpha_1 - h_1)),
    Eq(omega_1, m_1 + xi*(omega_1 - n_1))
]
l2 = [
    Eq(x_1, lambda_1 + zeta*(lambda_1 - y_2)),
    Eq(g_1, omega_1 + gamma*(omega_1 - h_2)),
    Eq(omega_1, lambda_1 + xi*(lambda_1 - n_2))
]
l3 = [
    Eq(x_1, (alpha_1*y_2 - lambda_1*y_1) / ((alpha_1 - lambda_1) - (y_1 - y_2))),
    Eq(g_1, (alpha_1*h_2 - omega_1*h_1) / ((alpha_1 - omega_1) - (h_1 - h_2))),
    Eq(omega_1, (omega_1*n_2 - lambda_1*n_1) / ((omega_1 - lambda_1) - (n_1 - n_2)))
]
l4 = [
    Eq(x_1, (lambda_1 + alpha_1 - omega_2) / (2*omega_1)),
    Eq(g_1, (omega_1 + alpha_1 - lambda_2) / (2*lambda_1)),
    Eq(omega_1, (lambda_1 + omega_1 - alpha_2) / (2*alpha_1))
]
l5 = [
    Eq(y_1, alpha_1 - 1/zeta*(alpha_1 - x_1)),
    Eq(h_1, alpha_1 - 1/gamma*(alpha_1 - g_1)),
    Eq(n_1, omega_1 - 1/xi*(omega_1 - m_1))
]
l6 = [
    Eq(y_1, lambda_2 - 1/zeta*(lambda_1 - x_1)),
    Eq(h_1, omega_2 - 1/gamma*(omega_1 - g_1)),
    Eq(n_1, lambda_2 - 1/xi*(lambda_1 - n_1))
]
l7 = [
    Eq(y_1, (x_1*lambda_2 - x_2*alpha_1) / ((lambda_2 - alpha_1) - (x_2 - x_1))),
    Eq(h_1, (g_1*omega_2 - g_2*alpha_1) / ((omega_2 - alpha_1) - (g_2 - g_1))),
    Eq(n_1, (m_1*lambda_2 - m_2*omega_1) / ((lambda_2 - omega_1) - (m_2 - m_1)))
]
l8 = [
    Eq(y_1, (lambda_2 + alpha_1 - omega_1) / (2*omega_2)),
    Eq(h_1, (omega_2 + alpha_1 - lambda_1) / (2*lambda_2)),
    Eq(n_1, (lambda_2 + omega_1 - alpha_1) / (2*alpha_2))
]
l9 = [
    Eq(alpha_1, x_1*omega_1 + omega_2*y_1),
    Eq(alpha_1, g_1*lambda_1 + lambda_2*h_1),
    Eq(omega_1, m_1*alpha_1 + alpha_2*n_1)
]
l10 = [
    Eq(alpha_1, omega_1*(x_1 - x_2) + lambda_2),
    Eq(alpha_1, lambda_1*(g_1 - g_2) + omega_2),
    Eq(omega_1, alpha_1*(m_1 - m_2) + lambda_2)
]
l11 = [
    Eq(alpha_1, omega_2*(y_1 - y_2) + lambda_1),
    Eq(alpha_1, lambda_2*(h_1 - h_2) + omega_1),
    Eq(omega_1, alpha_2*(n_1 - n_2) + lambda_1)
]
l12 = [
    Eq(alpha_1, (x_1*(y_2 - y_1) - lambda_1*(x_1 - y_1)) / (y_2 - x_1)),
    Eq(alpha_1, (g_1*(h_2 - h_1) - omega_1*(g_1 - h_1)) / (h_2 - g_1)),
    Eq(omega_1, (m_1*(n_2 - n_1) - lambda_1*(m_1 - n_1)) / (n_2 - m_1))
]
l13 = [
    Eq(lambda_1, x_1*omega_1 + omega_2*y_2),
    Eq(omega_1, g_1*lambda_1 + lambda_2*h_2),
    Eq(lambda_1, m_1*alpha_1 + alpha_2*n_2)
]
l14 = [
    Eq(lambda_1, omega_1*(x_1 - x_2) + alpha_2),
    Eq(omega_1, lambda_1*(g_1 - g_2) + alpha_2),
    Eq(lambda_1, alpha_1*(m_1 - m_2) + omega_2)
]
l15 = [
    Eq(lambda_1, (alpha_1*(x_1 - y_2) - x_1*(y_1 - y_2)) / (x_1 - y_1)),
    Eq(omega_1, (alpha_1*(g_1 - h_2) - g_1*(h_1 - h_2)) / (g_1 - h_1)),
    Eq(lambda_1, (omega_1*(m_1 - n_2) - m_1*(n_1 - n_2)) / (m_1 - n_1))
]
l16 = [
    Eq(lambda_1, omega_2*(y_2 - y_1) + alpha_1),
    Eq(omega_1, lambda_2*(h_2 - h_1) + alpha_1),
    Eq(lambda_1, alpha_2*(n_2 - n_1) + omega_1)
]
l17 = [
    Eq(zeta, (x_1 - alpha_1) / (alpha_1 - y_1)),
    Eq(omega_1, (y_1 - alpha_1) / (y_1 - x_1)),
    Eq(gamma, (g_1 - alpha_1) / (alpha_1 - h_1)),
    Eq(xi, (omega_1 - m_1) / (m_1 - n_1))
]
l18 = [
    Eq(zeta, (lambda_1 - alpha_1) / (alpha_1 - y_2)),
    Eq(omega_1, (omega_1 - g_1) / (g_1 - h_2)),
    Eq(gamma, (g_1 - omega_1) / (omega_1 - h_1)),
    Eq(xi, (lambda_1 - n_1) / (n_2 - n_1))
]
l19 = [
    Eq(zeta, (x_1*lambda_2 - x_2*alpha_1) / ((lambda_2 - alpha_1) - (x_2 - x_1))),
    Eq(omega_1, (g_1*omega_2 - g_2*alpha_1) / ((omega_2 - alpha_1) - (g_2 - g_1))),
    Eq(gamma, (g_1*lambda_2 - g_2*lambda_1) / ((lambda_2 - lambda_1) - (g_2 - g_1))),
    Eq(xi, (m_1*lambda_2 - m_2*alpha_1) / ((lambda_2 - alpha_1) - (m_2 - m_1)))
]
l20 = [
    Eq(zeta, (lambda_2 + alpha_1 - omega_1) / (2*omega_2)),
    Eq(omega_1, (lambda_2 + omega_1 - alpha_1) / (2*alpha_2)),
    Eq(gamma, (omega_2 + alpha_1 - lambda_1) / (2*lambda_2)),
    Eq(xi, (lambda_2 + omega_1 - alpha_2) / (2*alpha_1))
]
##########################################################################################################################
modvarlist = [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19, l20]
moddes = {}
moddel = {}
dfs = [] 
dfl = [] 
parv = 'empty'

for i in range(len(modvarlist)):
    moddes[tuple(parv)] = dfs
    moddel[tuple(parv)] = dfl
    parv = modvarlist[i]
    
    for j in range(len(modvarlist[i])):
        dfs.append(str(modvarlist[i][j]))
        dfl.append(latex(modvarlist[i][j]))

moddes[tuple(parv)] = dfs
moddel[tuple(parv)] = dfl
#len(dfl)
#len(moddel)
