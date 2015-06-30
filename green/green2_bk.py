# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pandas as pd
import numpy as np


dt     = 1.0
NLOOP  = 100
nparam = 3

#initial           v0   k/m  myu
param  = np.array([3.0, 0.1, 0.02])
eparam = np.array([0.2, 0.05, 0.005])
delta  = np.array([0.1, 0.02, 0.002])


obs  = pd.read_csv('obs.csv')
obs  = obs.x
nobs = len(obs)
eobs = 0.5


def model(param):
    v0, k_m, myu = param

    x = np.zeros(nobs)
    v = np.zeros(nobs)

    x[0] = 0.0
    v[0] = v0

    for i in xrange(nobs-1):
        if -3 <= x[i] < 20:
            F = -k_m * max(x[i]-10, 0)
        else:
            break

        if v[i] > 0:
            a = -myu * 9.8 + F
        else:
            a = myu * 9.8 + F

        v[i+1] = v[i] + a * dt
        x[i+1] = x[i] + v[i+1] * dt
    return x


G  = np.zeros((nobs, nparam))
B  = np.zeros((nparam, nparam))
R  = np.zeros((nobs, nobs))

for i in xrange(nparam):
    B[i,i] = 1.0/(eparam[i])**2
for i in xrange(nobs):
    R[i,i] = 1.0/(eobs)**2

Bin = np.linalg.inv(B)
Rin = np.linalg.inv(R)


Ga_0 = model(param)
Ga_b = Ga_0.copy()


Jb = np.zeros(NLOOP+1)
Jo = np.zeros(NLOOP+1)

Jb[0] = 0
Jo[0] = np.dot(np.dot( (Ga_0-obs).T,Rin ),(Ga_0-obs) ) * 0.5


ax1 = plt.subplot(2,1,1)
ax2 = plt.subplot(2,1,2)

ax1.plot(obs, 'o', label='obs')
ax1.plot(Ga_0, 'k--', label='background')


for i in range(NLOOP):

    print i, param

    for j in range(nparam):
        cff    = param.copy()
        cff[j] += eparam[j]
        Ga     = model(cff)
        G[:,j] = (Ga - Ga_b) / delta[j]

    cff1   = np.linalg.inv( Bin + np.dot( np.dot(G.T,Rin), G) )
    cff2   = np.dot( np.dot(G.T,Rin), (Ga_b - obs) )
    dparam = np.dot( -cff1,cff2 )

    param += dparam
    Ga_b   = model(param)

    Jb[i+1] = np.dot(np.dot( dparam.T,Bin ),dparam ) * 0.5
    Jo[i+1] = np.dot(np.dot( (Ga_b-obs).T,Rin ),(Ga_b-obs) ) * 0.5

    ax1.plot(Ga_b, c=cm.jet(float(i)/NLOOP,1), label='iter{}'.format(i+1))


J = Jb + Jo
ax2.plot(np.log10(J), label='J')
ax2.plot(np.log10(Jo), label='Jo')

#ax1.legend()
ax2.legend()
#plt.show()
plt.savefig('green2.png')
