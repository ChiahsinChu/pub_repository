
from ase.io import read
import numpy as np

from jxzhu_package.postprocess import common


NA = 6.02214076E+23
ANG_TO_CM = 1E-08


atoms = read("./input_example/coord.xyz")
atoms.set_cell([16.869, 16.869, 41.478, 90, 90, 120])
idx = common.get_elem_idxs(atoms, "O")
z_coords = common.get_idx_coord(atoms, idx, axis="z")
nbins = 20
mol_mass = 18

count, _x = np.histogram(z_coords, bins=nbins)
print(_x)
print(count)
count = np.array(count)
cross_area = common.cell_cross_area(atoms)
v = (_x[1] - _x[0]) * cross_area
den = (count / NA * mol_mass) / (v * ANG_TO_CM ** 3)
#den = interface._get_den(count, v, mol_mass)
x = []
for i in range(len(_x)-1):
    x.append((_x[i] + _x[i+1]) / 2)

print(x)
print(den)