# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pandas as pd
import numpy as np

""" parameters """

NLOOP  = 5
nparam = 2

obsfile = 'obs.csv'
parfile = 'green_param.h'
outfile = 'out.csv'

pname  = ['KDOMs', 'KPOMs']
param  = [5.0e-08, 1.2e-09]
eparam = [5.0e-09, 1.2e-10]
delta  = [5.0e-09, 1.2e-10]

eobs   = {'NH4':50,'PO4':5}

""" functions """

def H(x):
    y = np.zeros(nobs)
    for iobs in range(nobs):
        time  = obs.time[iobs]
        depth = obs.depth[iobs]
        varid = obs.varid[iobs]
        y[iobs] = x[time, depth, varid]
    return y

def run(param):
    with open(parfile, 'w') as f:
        for i in range(nparam):
            param_str = '{} = {}'.format(pname[i], param[i])
            f.write(param_str)

    os.system("make green")
    print 'model running...'
    os.system("./a.out > log.txt")

    x = pd.read_csv(outfile)
    return H(x)

""" prepare """

obs  = pd.read_csv(obsfile)
nobs = len(obs)

Jb = np.zeros(NLOOP+1)
Jo = np.zeros(NLOOP+1)
G  = np.zeros((nobs, nparam))
B  = np.zeros((nparam, nparam))
R  = np.zeros((nobs, nobs))

for i in xrange(nparam):
    B[i,i] = 1.0/(eparam[i])**2
for i in xrange(nobs):
    R[i,i] = 1.0/(eobs[obs.name[i]])**2

Bin = np.linalg.inv(B)
Rin = np.linalg.inv(R)

""" start first run """

Ga_0 = run(param)
Ga_b = Ga_0.copy()

Jb[0] = 0
Jo[0] = np.dot(np.dot( (Ga_0-obs).T,Rin ),(Ga_0-obs) ) * 0.5

ax1 = plt.subplot(2,1,1)
ax2 = plt.subplot(2,1,2)

ax1.plot(obs, 'o', label='obs')
ax1.plot(Ga_0, 'k--', label='base')

""" start runs """

for i in range(NLOOP):
    print i, param

    for j in range(nparam):
        cff    = param.copy()
        cff[j] += eparam[j]
        Ga     = run(cff)
        G[:,j] = (Ga - Ga_b) / delta[j]

    cff1   = np.linalg.inv( Bin + np.dot( np.dot(G.T,Rin), G) )
    cff2   = np.dot( np.dot(G.T,Rin), (Ga_b - obs) )
    dparam = np.dot( -cff1,cff2 )

    param += dparam
    Ga_b   = run(param)

    Jb[i+1] = np.dot(np.dot( dparam.T,Bin ),dparam ) * 0.5
    Jo[i+1] = np.dot(np.dot( (Ga_b-obs).T,Rin ),(Ga_b-obs) ) * 0.5

    ax1.plot(Ga_b, c=cm.jet(float(i)/NLOOP,1), label='iter{}'.format(i+1))

""" finalize """

J = Jb + Jo
ax2.plot(np.log10(J), label='J')
ax2.plot(np.log10(Jo), label='Jo')

ax1.legend()
ax2.legend()
plt.savefig('green2.png')
