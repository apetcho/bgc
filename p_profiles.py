# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm

na_values = ['             NaN', '****************']

df = pd.read_csv('bgc_out.csv', index_col=['time'], na_values=na_values)

tmax = int(max(df.index)/150)*150

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

    for t in range(0,tmax+1,tmax/5):
        plt.plot(df[name][t], -df.z[0],
                 label='{}year'.format(t/365),
                 c=cm.jet(float(t)/tmax,1))

    vmax = df[name].max()

    plt.title(name)
    plt.xticks([vmax])
    plt.xlim(-0.1*vmax, 1.1*vmax)
    plt.ylim(-23, 1.5)

    if not name in ['temp', 'POMs']:
       plt.tick_params(labelleft='off')

    if name == 'DOMs':
    	plt.legend(loc=3)

#plt.show()
plt.savefig('bgc_out_profiles.png', bbox_inches='tight')
