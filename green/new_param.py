# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv('out/new_param.csv', index_col='pname')
df = df.T


a = 0
plt.figure(figsize=[12,10])
for name in df.columns:

	a += 1
	plt.subplot(5,3,a)

	plt.plot(df[name])
	plt.title(name)

plt.savefig('out/new_param.png')