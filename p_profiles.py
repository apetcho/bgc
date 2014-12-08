# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm



csvfile = 'out.csv'
pngfile = 'out_profiles.png'



na_values = ['             NaN', '****************']

df = pd.read_csv(csvfile, index_col=['time'], na_values=na_values)

tmax = int(max(df.index)/30)*30
#tmax = int(max(df.index))
print tmax, tmax-360

plt.figure(figsize=(16, 8))

a=0
for name in df.columns[1:]:

    print a, name
    if name == 'CH4':
        continue
    elif 'Unnamed' in name:
        break

    a+=1
    ax = plt.subplot(2, 11, a)

    for t in range(tmax-360,tmax+1,60):
        plt.plot(df[name][t], -df.z[0],
                 label='{}days'.format(t-tmax+360),
                 c=cm.jet(float(t-tmax+360)/360,1))

    vmax = df[name][tmax-360:].max()

    plt.title(name)
    plt.xticks([vmax])
    plt.xlim(-0.1*vmax, 1.1*vmax)
    plt.ylim(-11, 1)

    if not name in ['temp', 'POMs']:
       plt.tick_params(labelleft='off')

    if name == 'DOMs':
    	plt.legend(loc=3)

#plt.show()
plt.savefig(pngfile, bbox_inches='tight')
