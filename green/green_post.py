# -*- coding: utf-8 -*-

import os
import glob
import pandas as pd


result = {}

obs = pd.read_csv('obs2.csv')
depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
obs['depth'] = obs['layer'].map(depths)
obs = obs[obs.station<=2]
print obs

outfiles = glob.glob('green_out2/*')
#outfiles = os.listdir('green_out/')

for outfile in outfiles:
	print outfile
	out = pd.read_csv(outfile, parse_dates='time', index_col=['time','z'])

	cost = 0
	for i in obs.index:
		t = int(obs.time[i]/30)*30
		dep = obs.depth[i]
		NH4 = obs.NH4[i]
		sigma = 100.0
		cff = 1/(sigma**2)
		cost += cff*(out['NH4'][t][dep]-NH4)**2

	result[outfile[11:-4]] = cost

result = pd.Series(result)

print result.sort()

result.to_csv('result2.csv')
