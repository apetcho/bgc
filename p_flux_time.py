# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm

na_values = ['             NaN', '****************']

df = pd.read_csv('out_flux.csv', index_col=['time'], na_values=na_values)

df.describe().to_csv('flux_describe.csv')

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

    v = df[name]

    time = v.index.tolist()
    plt.plot([t/365 for t in time], v.tolist())

    plt.title(name)

plt.subplots_adjust(hspace=0.4, wspace=0.3)
#plt.show()
plt.savefig('out_flux_time.png', bbox_inches='tight')
