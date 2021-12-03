import numpy as np
from ase import io

from jxzhu_package.postprocess import common

NA = 6.02214076E+23
ANG_TO_CM = 1E-08


def _get_den(count, v, mol_mass):
    """
    TBC
    """
    den = (count / NA * mol_mass) / (v * ANG_TO_CM ** 3)
    return den


def get_den(atoms, z_coords, zlo, zhi, mol_mass):
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

    count  = 0
    for coord in z_coords:
        if coord >= zlo and coord < zhi:
                count  = count + 1
    #den = count * mol_mass * 10 / v_box / 6.02214076
    den = _get_den(count, v_box, mol_mass)
    return count, den
    
def get_den_hist(atoms, z_coords, nbins, mol_mass):
    """
    TBC
    """
    #z = atoms.get_cell_lengths_and_angles()[2]
    #w_bin = z / nbins
    #x = np.arange(nbins) * w_bin + w_bin / 2
    count, _x = np.histogram(z_coords, bins=nbins)
    #print(count)
    count = np.array(count)
    cross_area = common.cell_cross_area(atoms)
    v = (_x[1] - _x[0]) * cross_area
    den = _get_den(count, v, mol_mass)
    x = []
    for i in range(len(_x)-1):
        x.append((_x[i] + _x[i+1]) / 2)
    return x, den


def get_wat_den_hist(atoms, nbins):
    """
    TBC
    """
    idx = common.get_elem_idxs(atoms, "O")
    z_coords = common.get_idx_coord(atoms, idx, axis="z")
    x, den = get_den_hist(atoms, z_coords, nbins, mol_mass=18)
    return x, den

def get_bulk_wat_den(atoms, z_wat):
    idx = common.get_elem_idxs(atoms, "O")
    z_coords = common.get_idx_coord(atoms, idx, axis="z")
    zlo = (atoms.cell.cellpar()[2] - z_wat) / 2
    zhi = (atoms.cell.cellpar()[2] + z_wat) / 2
    den = get_den(atoms, z_coords, zlo, zhi, mol_mass=18)