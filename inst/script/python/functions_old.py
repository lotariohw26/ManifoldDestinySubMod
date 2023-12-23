import sympy
from sympy import solve, Eq, symbols, latex, simplify, diff, poly, sympify, Matrix, pprint
def rall(sel=[0, 0, 0]):
    n, m = symbols('n m')
    
    rxy = Matrix([[m, -n, 0], 
                  [n, m, 0],
                  [0, 0, 1]])
    
    rxz = Matrix([[m, 0,-n], 
                  [0, 1, 0],
                  [n, 0, m]])
    
    ryx = Matrix([[m, n, 0], 
                  [-n, m, 0],
                  [0, 0, 1]])
    
    ryz = Matrix([[1, 0, 0], 
                  [0, m, -n],
                  [0, n, m]])
    
    rzx = Matrix([[m, 0, n], 
                  [0, 1, 0],
                  [-n, 0, m]])
    
    rzy = Matrix([[1, 0, 0], 
                  [0, m, n],
                  [0, -n, m]])
    
    ps = [0,rxy, rxz, ryx, ryz, rzx, rzy]
    
    allrot = [ps[i] for i in sel]
    
    return allrot

#def genpolycoeff(expr='k0 + k1*g + k2*h + k3*g**2 + k4*h**2 + k5*g*h', solvd='alpha', solvf='g',eur=[0, 0, 0]):
#def genpolycoeff(expr='k0+k1*x+k2*y', solvd='alpha', solvf='y', eur=[0, 0, 0]):
def genpolycoeff(expr='k0+k1*x+k2*y', solvd='z', solvf='ui', eur=[1, 4, 2]):
    sympy.var('alpha x y g h n m zeta Gamma lamda ui')
    x, y, z = sympy.symbols('x y z')
    sum_eur = sum(eur)
    if sum_eur == 0:
        print('if')
        exprc = expr + '-' + solvd
        asexpr = sympify(exprc)
        polys = poly(asexpr, sympify(solvf)).all_coeffs()
        abc = []
        uvw = []
        ABCDE = [0, 0, 0, 0, 0]
        ABCDE[:len(polys)] = polys
        return ABCDE, abc, uvw
    else:
        ui, vi, wi = sympy.symbols('ui vi wi')
        u0, v0, w0 = sympy.symbols('u0 v0 w0')
        u1, v1, w1 = sympy.symbols('u1 v1 w1')
        u2, v2, w2 = sympy.symbols('u2 v2 w2')
        x,  y,  z  = sympy.symbols('x y z')
        m1, m2, m3 = sympy.symbols('m1 m2 m3')
        n1, n2, n3 = sympy.symbols('n1 n2 n3')
        mu, mv, mw = sympy.symbols('mu mv mw')
        # Defining
        PS = rall(sel=eur)
        r0 = Matrix([u0, v0, w0])
        r1 = PS[0].subs([(n, n1), (m, m1)]) * r0
        r2 = PS[1].subs([(n, n2), (m, m2)]) * r1
        RR = PS[2].subs([(n, n3), (m, m3)]) * r2
        sympy.expand(RR.row(0)[0])
        sympy.expand(RR.row(1)[0])
        sympy.expand(RR.row(2)[0])
        av = sympy.collect(sympy.expand(RR.row(0)[0]), (u0, v0, w0))
        bv = sympy.collect(sympy.expand(RR.row(1)[0]), (u0, v0, w0))
        cv = sympy.collect(sympy.expand(RR.row(2)[0]), (u0, v0, w0))
        a1 = av.coeff(u0)
        a2 = av.coeff(v0)
        a3 = av.coeff(w0)
        b1 = bv.coeff(u0)
        b2 = bv.coeff(v0)
        b3 = bv.coeff(w0)
        c1 = cv.coeff(u0)
        c2 = cv.coeff(v0)
        c3 = cv.coeff(w0)
        Eu = Eq(x, a1 * u0 + a2 * v0 + a3 * w0)
        Ev = Eq(y, b1 * u0 + b2 * v0 + b3 * w0)
        Ew = Eq(z, c1 * u0 + c2 * v0 + c3 * w0)
        Eu = Eq(x, a1 * u0 + a2 * v0 + a3 * w0).subs([(u0,ui-mu),(v0,vi-mv),(w0,wi-mw)])
        Ev = Eq(y, b1 * u0 + b2 * v0 + b3 * w0).subs([(u0,ui-mu),(v0,vi-mv),(w0,wi-mw)])
        Ew = Eq(z, c1 * u0 + c2 * v0 + c3 * w0).subs([(u0,ui-mu),(v0,vi-mv),(w0,wi-mw)])
        stri = ""
        strk = [" k" + str(i) for i in range(0, 30)]
        stri.join(stri)
        exprc = expr + '-' + solvd
        asexpr = sympify(exprc)
        asexpr2 = asexpr.subs([(x, solve(Eu, x)[0]), (y, solve(Ev, y)[0]), (z, solve(Ew, z)[0])])
        polys = poly(asexpr2, sympify(solvf)).all_coeffs()
        abc = [a1, a2, a3, b1, b2, b3, c1, c2, c3]
        uvw = [Eu, Ev, Ew]
        ABCDE = [0, 0, 0, 0, 0]
        ABCDE[:len(polys)] = polys
        return ABCDE, abc, uvw


genpolycoeff()[0]



