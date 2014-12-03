# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm



csvfile = 'out.csv'
pngfile = 'out_time.png'



na_values = ['             NaN', '****************']
df = pd.read_csv(csvfile, index_col=['time', 'z'], na_values=na_values)



plt.figure(figsize=(14, 9))
a = 0

for name in df.columns:

    print a, name

    if name == 'CH4':
        continue
    elif 'Unnamed' in name:
        break

    a += 1
    ax = plt.subplot(5, 5, a)

    v = df[name].unstack('z')

    for z in v.columns:
        time = v[z].index.tolist()
        plt.plot([t/365 for t in time], v[z].tolist(), c=cm.jet(z / 21))

    plt.title(name)

plt.subplots_adjust(hspace=0.4, wspace=0.3)
#plt.show()
plt.savefig(pngfile, bbox_inches='tight')
