# -*- coding: utf-8 -*-


import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import os
from bgc_userconfig import *


def plot_flux(csvfile, pngfile):

    print '\nplot_flux'
    print csvfile
    df = pd.read_csv(csvfile, index_col=['time'])

    plt.figure(figsize=(16, 11))

    a = 0
    for name in df.columns:

        print name,
        if name == 'CH4':
            continue
        elif 'Unnamed' in name:
            break

        a += 1
        ax   = plt.subplot(6, 6, a)
        v    = df[name]
        time = v.index.tolist()

        plt.plot([t/360 for t in time], v.tolist())
        plt.title(name)

    print ''
    print pngfile 
    plt.subplots_adjust(hspace=0.4, wspace=0.3)
    plt.savefig(pngfile, bbox_inches='tight')


def make_avg(csvfile, avgfile):

    print '\nmake_avg'
    print csvfile
    df = pd.read_csv(csvfile, index_col=['time'])
    last = df.ix[df.index[-360:-1], :]
    mean = last.mean()

    print avgfile 
    mean.to_csv(avgfile)


if __name__ == '__main__':

    config = bgc_userconfig()

    for point in [1, 2]:

        outdir  = config.outdir
        csvfile = os.path.join(outdir, 'out_flux{}.csv'.format(point) )
        pngfile = os.path.join(outdir, 'plot_flux{}.png'.format(point) )
        avgfile = os.path.join(outdir, 'avg_flux{}.csv'.format(point) )

        plot_flux(csvfile, pngfile)
        make_avg(csvfile, avgfile)
