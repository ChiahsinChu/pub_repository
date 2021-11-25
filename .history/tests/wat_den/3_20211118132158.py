from ase.io import iread

from jxzhu_package.postprocess import interface


nbins = 20
wat_den = []

for atoms in iread("./input_example/reftraj.xyz"):
    atoms.set_cell([16.869, 16.869, 41.478, 90, 90, 120])


x, den = interface.get_wat_den_hist(atoms, nbins)
