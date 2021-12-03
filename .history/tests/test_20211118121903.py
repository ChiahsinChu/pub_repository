from ase.io import read

from jxzhu_package.postprocess import interface, common

atoms = read("./input_example/coord.xyz")
idx = common.get_elem_idxs(atoms, "O")
z_coords = common.get_idx_coord(atoms, idx, axis="z")
x, den = interface.get_den_hist(atoms, z_coords, nbins=20, mol_mass=18)

print(x)
print(den)


