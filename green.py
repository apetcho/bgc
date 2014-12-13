# -*- coding: utf-8 -*-

import os
import pandas as pd



params = pd.read_csv('green_params.csv', index_col='name')

params['min'] = params['min'].fillna(value=params['mean']/2)
params['max'] = params['max'].fillna(value=params['mean']*2)
print params

os.system("make green")

for name, param in params['min'].iteritems():
	param_str = '{} = {}'.format(name, param)

	f = open('green_param.h', 'w')
	f.write(param_str)
	f.close()

	os.system("./a.out")
