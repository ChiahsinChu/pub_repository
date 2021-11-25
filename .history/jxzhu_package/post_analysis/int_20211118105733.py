import numpy as np
from ase import io

from jxzhu_package.postprocess import common

def get_den(atoms, idx, zlo, zhi):
    cross_area = common.cell_cross_area(atoms)
    v_box = cross_area * (zhi - zlo)
    _coords = atoms.get_positions()
    z_coords = _coords[:,2]
    coords = []
    for i in idx:
        coords.append(z_coords[i])

    den = []
    for data in pos:
        count  = 0
        for pos in data:
            if pos >= zlo and pos < zhi:
                count  = count + 1
        den.append(count*18*10/v_box/6.02)
    
    return count, np.mean(wat_den), np.std(wat_den)
    
def _wat_den_hist(atoms, nbins):
    z = atoms.get_cell_lengths_and_angles()[2]
    w_bin = z / nbins
    x = np.arange(nbins) * w_bin + w_bin / 2
    cross_area = common.cell_cross_area(atoms)
    wat_den = np.histogram(a,bins=[0,0.2,0.5,0.8,1])
    return x, wat_den


def wat_den_hist(atoms, nbins, surf_min, surf_):
    
