# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import os
from bgc_userconfig import *


def plot_profiles(outdir, outfile, pngfile, obsfile):

    outfile = os.path.join(outdir, outfile)
    pngfile = os.path.join(outdir, pngfile)

    print outfile
    out = pd.read_csv(outfile, index_col=['time'])

    print obsfile
    obs = pd.read_csv(obsfile)
    
    depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
    obs['depth'] = obs['layer'].map(depths)

    tmax = int(max(out.index)/360)*360

    plt.figure(figsize=(16, 8))

    a=0
    for name in out.columns[1:]:

        print name,
        if name == 'CH4':
            continue
        elif 'Unnamed' in name:
            break

        a+=1
        ax = plt.subplot(2, 11, a)

        for t in range(tmax/2,tmax+1,tmax/6):
            plt.plot(out[name][t], -out.depth[0],
                     label='{}years'.format(t/360),
                     c=cm.jet(float(t)/tmax,1))

        if point == 1:
            pobs = obs[obs.station<=2]
        elif point == 2:
            pobs = obs[obs.station>=4]

        if name in pobs.columns:
            plt.plot(pobs[name], -pobs['depth'], 'o')

        vmax = out[name][tmax/2:].max()

        plt.title(name)
        plt.xticks([vmax])
        plt.xlim(-0.1*vmax, 1.1*vmax)
        plt.ylim(-17, 1)

        if not name in ['O2', 'POMs']:
           plt.tick_params(labelleft='off')

        if name == 'temp':
            plt.legend(loc=4)

    print ''
    print pngfile
    plt.savefig(pngfile, bbox_inches='tight')


if __name__ == '__main__':

    config = bgc_userconfig()

    for point in [1, 2]:

        obsfile = config.obsfile
        outdir  = config.outdir
        outfile = 'out{}.csv'.format(point)
        pngfile = 'plot_profile{}.png'.format(point)

        plot_profiles(outdir, outfile, pngfile, obsfile)
