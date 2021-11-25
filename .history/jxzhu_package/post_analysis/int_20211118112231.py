import numpy as np
from ase import io

from jxzhu_package.postprocess import common

NA = 6.02214076E+23
ANG_TO_CM = 1E-08


#TODO: input coord (e.g., mass center)
def get_den(atoms, idx, zlo, zhi, mol_mass):
    """
    Args:
        atoms: ASE Atoms object
        idx: indices list of species for analysis
    Returns:
        count: 
        den: 

    """
    cross_area = common.cell_cross_area(atoms)
    v_box = cross_area * (zhi - zlo)
    z_coords = atoms.get_positions()[:,2]
    coords = []
    for i in idx:
        coords.append(z_coords[i])

    count  = 0
    for coord in coords:
        if coord >= zlo and coord < zhi:
                count  = count + 1
    #den = count * mol_mass * 10 / v_box / 6.02214076
    den = (count / NA * mol_mass) / (v_box * ANG_TO_CM ** 3)
    return count, den
    
def _wat_den_hist(atoms, nbins):
    z = atoms.get_cell_lengths_and_angles()[2]
    w_bin = z / nbins
    x = np.arange(nbins) * w_bin + w_bin / 2
    cross_area = common.cell_cross_area(atoms)
    wat_den = np.histogram(a,bins=[0,0.2,0.5,0.8,1])
    return x, wat_den


def wat_den_hist(atoms, nbins, surf_min, surf_):
    
