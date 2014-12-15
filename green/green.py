# -*- coding: utf-8 -*-

import os
import pandas as pd
import numpy as np


#params = pd.read_csv('green_params.csv', index_col='name')
#params['min'] = params['min'].fillna(value=params['mean']/2)
#params['max'] = params['max'].fillna(value=params['mean']*2)

params = pd.read_csv('green_params2.csv', index_col='name')

print params

os.mkdir("green_out2")
#os.mkdir("green_flux")
#os.mkdir("green_circulations")

for name in params.index:

	for i in range(5):

		ini = params['ini'][name]
		std = params['std'][name]
		param = ini + std * (1-np.random.random()*2)
		param_str = '{} = {}'.format(name, param)

		f = open('green_param.h', 'w')
		f.write(param_str)
		f.close()

		os.system("make green")
		os.system("./a.out")
		os.system("cp out.csv green_out2/{}_{}.csv".format(name, param))
		#os.system("cp out_flux.csv green_flux/{}_{}.csv".format(name, param))
		#os.system("cp out_circulations.csv green_circulations/{}_{}.csv".format(name, param))

