import numpy as np
from ase.io import read

from jxzhu_package.postprocess import interface, common

atoms = read("./input_example/coord.xyz")
coords = common.get_elem_idxs(atoms, elem_type)

