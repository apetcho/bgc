# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm



#outfile = 'out.csv'
outfile = 'green/green_out2/ratio_CN_6.54899999482.csv'
obsfile = 'green/obs.csv'
pngfile = 'out_profiles.png'



out = pd.read_csv(outfile, index_col=['time'])
obs = pd.read_csv(obsfile)

tmax = int(max(out.index)/30)*30

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

    for t in range(tmax-360,tmax+1,60):
        plt.plot(out[name][t], -out.z[0],
                 label='{}days'.format(t-tmax+360),
                 c=cm.jet(float(t-tmax+360)/360,1))

    if name in obs.columns:
        print a, name, 'obs'
        plt.plot(obs[name], obs['depth'], 'o')

    vmax = out[name][tmax-360:].max()

    plt.title(name)
    plt.xticks([vmax])
    plt.xlim(-0.1*vmax, 1.1*vmax)
    plt.ylim(-17, 1)

    if not name in ['temp', 'POMs']:
       plt.tick_params(labelleft='off')

    if name == 'DOMs':
    	plt.legend(loc=3)

plt.show()
#plt.savefig(pngfile, bbox_inches='tight')
