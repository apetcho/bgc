# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm

pd.set_option('line_width', 200)

df = pd.read_csv('obs2.csv')

depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}

df['depth'] = df['layer'].map(depths)

s12 = df[df.station<=2]
s45 = df[df.station>=4]

for name in ['NH4', 'NO2+3', 'PO4', 'DOC']:

	plt.subplot(1,2,1)
	plt.title('s1,s2')
	plt.plot(s12[name], -s12.depth, 'o')
	plt.xlim([0,df[name].max()])

	plt.subplot(1,2,2)
	plt.title('s4,s5')
	plt.plot(s45[name], -s45.depth, 'o')
	plt.xlim([0,df[name].max()])

	plt.suptitle(name)
	plt.savefig('obs2_{}.png'.format(name))
	plt.clf()
	