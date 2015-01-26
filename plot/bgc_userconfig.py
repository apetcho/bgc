# -*- coding: utf-8 -*-

import pandas as pd


def bgc_userconfig():

	c = {}
	
	c['obsfile'] = '../input/obs.csv'
	c['outdir']  = '../output'
	
	return pd.Series(c)
