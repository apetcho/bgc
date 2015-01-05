# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm


plt.figure(figsize=(16, 8))
obsfile = 'in/obs2.csv'
outfile = ['in/bgc_rst1.csv', 'out/rst1.csv']
pngfile = 'out/profiles_ab.png'


obs = pd.read_csv(obsfile)
depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
obs['depth'] = obs['layer'].map(depths)


df = pd.read_csv('out/out1.csv', index_col='time')
depth = df.depth[df.index==0]


vmax={}
for i in range(2):

    out = pd.read_csv(outfile[i])

    a=0
    for name in out.columns:

        print a, name
        if name == 'CH4':
            continue
        elif 'Unnamed' in name:
            break

        a+=1
        ax = plt.subplot(2, 11, a)

        colors = ['b', 'r']
        labels = ['free', 'assim']
        plt.plot(out[name], -depth, label=labels[i])

        pobs = obs[obs.station<=2]

        if name in pobs.columns:
            print a, name, 'obs'
            plt.plot(pobs[name], -pobs['depth'], 'ko', label='obs')

        try:
            vmax[name] = max(out[name].max(),vmax[name])
        except:
            vmax[name] = out[name].max()

        plt.title(name)
        plt.xticks([vmax[name]])
        plt.xlim(-0.1*vmax[name], 1.1*vmax[name])
        plt.ylim(-17, 1)

        if not name in ['O2', 'POMs']:
            plt.tick_params(labelleft='off')

        if name == 'POMf':
        	plt.legend(loc=3)


plt.savefig(pngfile, bbox_inches='tight')
#plt.show()
