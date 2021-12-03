from ase.io import read

from jxzhu_package.postprocess import interface


atoms = read("./input_example/coord.xyz")
atoms.set_cell([16.869, 16.869, 41.478, 90, 90, 120])
nbins = 20

x, den = interface.get_wat_den_hist(atoms, nbins)

print(x)
print(den)