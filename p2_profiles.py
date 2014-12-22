# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm


plt.figure(figsize=(16, 8))
obsfile = 'obs.csv'
#outfile = 'out/out{}.csv'
#pngfile = 'out/out12_profiles.png'
outfile = 'green/out/out{}.csv'
pngfile = 'green/out/out12_profiles.png'


obs = pd.read_csv(obsfile)
depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
obs['depth'] = obs['layer'].map(depths)


vmax={}
for point in [1, 2]:


    out = outfile.format(point)
    out = pd.read_csv(out, index_col=['time'])


    tmax = int(max(out.index)/360)*360

    a=0
    for name in out.columns[1:]:

        print a, name
        if name == 'CH4':
            continue
        elif 'Unnamed' in name:
            break

        a+=1
        ax = plt.subplot(2, 11, a)

        colors = {1:'b', 2:'r'}
        plt.plot(out[name][tmax], -out.depth[0],
                     colors[point],
                     label='point{}'.format(point))

        if point == 1: 
            pobs = obs[obs.station<=2]
        elif point == 2: 
            pobs = obs[obs.station>=4]

        if name in pobs.columns:
            print a, name, 'obs'
            plt.plot(pobs[name], -pobs['depth'], 'o', c=colors[point])

        try:
            vmax[name] = max(out[name][tmax].max(),vmax[name])
        except:
            vmax[name] = out[name][tmax].max()

        plt.title(name)
        plt.xticks([vmax[name]])
        plt.xlim(-0.1*vmax[name], 1.1*vmax[name])
        plt.ylim(-17, 1)

        if not name in ['O2', 'POMs']:
            plt.tick_params(labelleft='off')

        if name == 'temp':
        	plt.legend(loc=4)


plt.savefig(pngfile, bbox_inches='tight')
#plt.show()
