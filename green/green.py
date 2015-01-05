# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import pandas as pd
import numpy as np
import os


""" parameters """

NLOOP = 5
LONG  = 10.0*360.0
SHORT = 10.0*360.0

tmpfile = 'param.tmp'

parfile = 'in/param.csv'
obsfile = 'in/obs.csv'
outfile = 'out/out1.csv'

newfile = 'out/green_new_param.csv'
cstfile = 'out/green_cost.csv'
grnfile = 'out/green.png'

#eobs = {'NH4':100.0,'PO4':1.0,'DOMs':500.0}
eobs = {'NH4':100.0,'PO4':10.0,'DOMs':500.0}


""" functions """

l2d = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}


def get_obs():

    """ return observations dataframe """

    data = pd.read_csv(obsfile)
    data = data[data.station <= 2]
    obs  = {'time':[], 'depth':[], 'name':[], 'value':[], 'error':[]}
    for i in data.index:
        for name in eobs.keys():
            obs['time' ].append( data.time[i] )
            obs['depth'].append( l2d[data.layer[i]] )
            obs['name' ].append( name )
            obs['value'].append( data[name][i] )
            obs['error'].append( eobs[name] )
    return pd.DataFrame(obs)


def H(x):

    """ return interpolated model x into observations space """

    y = np.zeros(nobs)
    for iobs in range(nobs):
        time  = obs.time[iobs]
        depth = obs.depth[iobs]
        name  = obs.name[iobs]
        y[iobs] = x[name][x.depth == depth].tolist()[-1]
    return y


def model(param, ndays):

    """ run model with parameters in argument """

    print param
    with open(tmpfile, 'w') as f:
        f.write('{}\n'.format(ndays))
        for i in range(nparam):
            param_str = '{}\n'.format(param[i])
            f.write(param_str)
    os.system("./a.out < {} > out/run.log".format(tmpfile))
    x = pd.read_csv(outfile)
    return H(x)


""" prepare runs """

par    = pd.read_csv(parfile,index_col='pname')
pname  = par.index
param  = par.param
eparam = param * par.perturb
nparam = len(param)

obs  = get_obs()
y    = obs.value
nobs = len(obs)

Jb = np.zeros(NLOOP+1)
Jo = np.zeros(NLOOP+1)
G  = np.zeros((nobs, nparam))
B  = np.zeros((nparam, nparam))
R  = np.zeros((nobs, nobs))

for i in xrange(nparam):
    B[i,i] = (eparam[i])**2
for i in xrange(nobs):
    R[i,i] = (obs.error[i])**2

Bin = np.linalg.inv(B)
Rin = np.linalg.inv(R)


""" start first run """

os.system("make green")

Ga_0 = model(param, LONG)
Ga_b = Ga_0.copy()

Jb[0] = 1
Jo[0] = np.dot(np.dot( (Ga_0-y).T,Rin ),(Ga_0-y) ) * 0.5

ax1 = plt.subplot(2,1,1)
ax2 = plt.subplot(2,1,2)

ax1.plot(y, 'o', label='obs')
ax1.plot(Ga_0, 'k--', label='base')


""" start runs """

params = {0:param[:]}
delta  = np.zeros(nparam)

for i in range(NLOOP):

    for j in range(nparam):
        print i, j,

        p      = param.copy()
        p[j]  += -eparam[j]
        Ga     = model(p+delta, SHORT)
        G[:,j] = (Ga - Ga_b) / -eparam[j]

    cff1   = np.linalg.inv( Bin + np.dot( np.dot(G.T,Rin), G) )
    cff2   = np.dot( np.dot(G.T,Rin), (Ga_b - y) )
    delta += np.dot( -cff1,cff2 )

    Ga_b   = model(param+delta, LONG)

    Jb[i+1] = np.dot(np.dot( delta.T,Bin ),delta ) * 0.5
    Jo[i+1] = np.dot(np.dot( (Ga_b-y).T,Rin ),(Ga_b-y) ) * 0.5

    ax1.plot(Ga_b, c=cm.jet(float(i)/NLOOP,1), label='iter{}'.format(i+1))

    params[i+1] = param + delta


""" finalize """

params = pd.DataFrame(params)
params.to_csv(newfile)

J    = Jb + Jo
cost = {'J':J, 'Jb':Jb, 'Jo':Jo}
cost = pd.DataFrame(cost)
cost.to_csv(cstfile)

ax2.plot(np.log10(J), label='J')
ax2.plot(np.log10(Jb), label='Jb')
ax2.plot(np.log10(Jo), label='Jo')

ax1.legend()
ax2.legend()
plt.savefig(grnfile)

os.system('python plot_profiles_ab.py')
