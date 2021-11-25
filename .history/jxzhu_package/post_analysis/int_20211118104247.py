import numpy as np
from ase import io

from jxzhu_package.postprocess import common

def bulk_wat_den(atoms, zlo, zhi):
    cross_area = common.cell_cross_area(atoms)
    v_box = cross_area * (zhi - zlo)
    o_pos = [atom.position[2] for atom in atoms if atom.symbol == 'O']

    wat_den = []
    for data in o_pos:
        count  = 0
        for pos in data:
            if pos > zlo and pos < zhi:
                count  = count + 1
 37     #wat_den.append(count*8.6666*10**(-3))
 38     wat_den.append(count*18*10/v_box/6.02)
  40 print(count)
 41 print(np.mean(wat_den))
 42 print(np.std(wat_den))
    
def wat_den_hist(atoms):
    cross_area = common.cell_cross_area(atoms)
