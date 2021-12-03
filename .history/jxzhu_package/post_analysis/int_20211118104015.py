import numpy as np
from ase import io

from jxzhu_package.postprocess import common

def bulk_wat_den(atoms, zlo, zhi):
    cross_area = common.cell_cross_area(atoms)
    
def wat_den_hist(atoms):
    cross_area = common.cell_cross_area(atoms)

    if triclinic == 1:
     23     v_box = box_size[0]*box_size[1]*box_size[2]*0.866
 24 else:
 25     v_box = box_size[0]*box_size[1]*box_size[2]
 26 
 27 o_pos = []
 28 for atoms in traj:
 29     o_pos.append([atom.position[2] for atom in atoms if atom.symbol == 'O'])
 30 
 31 wat_den = []
 32 for data in o_pos:
 33     count  = 0
 34     for pos in data:
 35         if pos > zlo and pos < zhi:
 36             count  = count + 1
 37     #wat_den.append(count*8.6666*10**(-3))
 38     wat_den.append(count*18*10/v_box/6.02)
 39 
 40 print(count)
 41 print(np.mean(wat_den))
 42 print(np.std(wat_den))