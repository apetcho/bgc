# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm


point = 1


outfile = 'out{}.csv'.format(point)
obsfile = 'obs.csv'
pngfile = 'out{}_profiles.png'.format(point)


out = pd.read_csv(outfile, index_col=['time'])
obs = pd.read_csv(obsfile)
depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
obs['depth'] = obs['layer'].map(depths)
obs = obs[obs.station<=2]


tmax = int(max(out.index)/360)*360


plt.figure(figsize=(16, 8))

a=0
for name in out.columns[1:]:

    print a, name
    if name == 'CH4':
        continue
    elif 'Unnamed' in name:
        break

    a+=1
    ax = plt.subplot(2, 11, a)

    for t in range(tmax/2,tmax+1,tmax/6):
        plt.plot(out[name][t], -out.z[0],
                 label='{}years'.format(t/360),
                 c=cm.jet(float(t)/tmax,1))

    if name in obs.columns:
        print a, name, 'obs'
        plt.plot(obs[name], -obs['depth'], 'o')


    vmax = out[name][tmax/2:].max()

    plt.title(name)
    plt.xticks([vmax])
    plt.xlim(-0.1*vmax, 1.1*vmax)
    plt.ylim(-17, 1)

    if not name in ['O2', 'POMs']:
       plt.tick_params(labelleft='off')

    if name == 'temp':
    	plt.legend(loc=4)


plt.savefig(pngfile, bbox_inches='tight')
#plt.show()
