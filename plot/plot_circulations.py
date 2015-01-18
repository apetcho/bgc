# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm


point = 1


csvfile = 'out_circulations{}.csv'.format(point)
pngfile = 'out_circulations{}.png'.format(point)
avgfile = 'avg_circulations{}.csv'.format(point)


df = pd.read_csv(csvfile, index_col=['time'])


plt.figure(figsize=(20, 15))

a = 0
for name in df.columns:

    print a, name
    if name == 'CH4':
        continue
    elif 'Unnamed' in name:
        break

    a += 1
    ax   = plt.subplot(11, 5, a)
    v    = df[name]
    time = v.index.tolist()

    plt.plot([t/360 for t in time], v.tolist())
    plt.title(name[:3])


plt.subplots_adjust(hspace=0.4, wspace=0.3)
plt.savefig(pngfile, bbox_inches='tight')
#plt.show()


last = df[len(df)-360:]
mean = last.mean()

mean.to_csv(avgfile)
