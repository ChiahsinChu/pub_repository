from ase.io import iread
import numpy as np

from jxzhu_package.postprocess import interface


nbins = 20
wat_den = []

for atoms in iread("../input_example/reftraj.xyz"):
    atoms.set_cell([16.869, 16.869, 41.478, 90, 90, 120])
    x, den = interface.get_wat_den_hist(atoms, nbins)
    wat_den.append(den)

#print(np.shape(wat_den))
wat_den = np.mean(wat_den, axis=0)
#print(np.shape(wat_den))
print(x)
print(wat_den)
output = [x,wat_den]
output = np.transpose(output)
print(output)



