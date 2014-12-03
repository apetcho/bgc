# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm


# files

csvfile = 'out_circulations.csv'

pngfile = 'out_circulations.png'

desfile = 'out_circulations_describe.csv'


# main

na_values = ['             NaN', '****************']

df = pd.read_csv(csvfile, index_col=['time'], na_values=na_values)

df.describe().to_csv(desfile)

plt.figure(figsize=(20, 15))

a = 0

for name in df.columns:

    print a, name

    if name == 'CH4':
        continue
    elif 'Unnamed' in name:
        break

    a += 1
    ax = plt.subplot(11, 5, a)

    v = df[name]

    time = v.index.tolist()
    plt.plot([t/365 for t in time], v.tolist())

    plt.title(name[:3])

plt.subplots_adjust(hspace=0.4, wspace=0.3)
#plt.show()
plt.savefig(pngfile, bbox_inches='tight')
