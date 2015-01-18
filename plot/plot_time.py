# -*- coding: utf-8 -*-

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import os

def plot_time(fig, csvfile):

    print csvfile
    df = pd.read_csv(csvfile, index_col=['time', 'depth'])

    a = 0
    for name in df.columns:

        if name == 'CH4':
            continue
        elif 'Unnamed' in name:
            break

        a += 1
        ax = plt.subplot(5, 5, a)
        v  = df[name].unstack('depth')

        for z in v.columns:
            time = v[z].index.tolist()
            plt.plot([t/365 for t in time], v[z].tolist(), c=cm.jet(z / 21))

        plt.title(name)

    return fig

if __name__ == '__main__':

    point = 1

    outdir  = '../output'
    csvfile = os.path.join(outdir, 'out{}.csv'.format(point))
    pngfile = os.path.join(outdir, 'out{}_time.png'.format(point))

    fig = plt.figure(figsize=(14, 9))
    fig = plot_time(fig, csvfile)

    print pngfile
    #plt.subplots_adjust(hspace=0.4, wspace=0.3)
    fig.tight_layout()
    plt.savefig(pngfile, bbox_inches='tight')