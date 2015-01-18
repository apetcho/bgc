# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib.ticker import MaxNLocator
import os

def make_fig(outfile):

    fig   = plt.figure(figsize=(14, 8))
    out   = pd.read_csv(outfile, index_col=['time'])

    for i, name in enumerate(out.columns[1:]):

        if 'Unnamed' in name:
            break

        ax = fig.add_subplot(3,8,i+1)

    return fig

def plot_profile_obs(fig, obsfile):

    obs = pd.read_csv(obsfile)
    depths = {1:1.0, 2:2.89, 3:4.84, 4:7.29, 5:12.25, 6:16.0}
    obs['depth'] = obs['layer'].map(depths)

def plot_profile_model(fig, outfile, time, color, label):

    print outfile
    out   = pd.read_csv(outfile, index_col=['time'])
    depth = out.depth[0]

    for i, name in enumerate(out.columns[1:]):

        if 'Unnamed' in name:
            break

        ax = fig.axes[i]
        value = out[name][time]
        ax.plot(value, -depth, c=color, label=label)

        ax.set_title(name)
        #plt.xticks([vmax])
        #plt.xlim(-0.1*vmax, 1.1*vmax)
        ax.set_ylim(-17, 1)

        if name == 'temp':
        	ax.legend(loc=4)

        ax.xaxis.set_major_locator( MaxNLocator(1) )

    return fig

if __name__ == '__main__':

    obsfile = '../input/obs.csv'
    outdir  = '../output'
    outfile = os.path.join(outdir, 'last{}.csv')
    pngfile = os.path.join(outdir, 'out_profiles.png')

    fig = make_fig(outfile.format(1))
    fig = plot_profile_model(fig, outfile.format(1), time=360, color='r', label='point1')
    fig = plot_profile_model(fig, outfile.format(2), time=360, color='b', label='point2')

    print pngfile
    fig.tight_layout()
    plt.savefig(pngfile, bbox_inches='tight')
