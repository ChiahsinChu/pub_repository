from ase import Atoms, io, build
import numpy as np
import os


def get_energies(energy_file="energy.log"):
    energies = np.loadtxt(energy_file)[:, 2]
    return energies


def get_coords_and_forces(dump_file="./dump.lammpstrj"):
    traj = io.read(dump_file, index=":")
    coords = []
    forces = []
    for atoms in traj:
        forces.append(atoms.get_forces())
        coords.append(atoms.get_positions())
    coords = np.reshape(coords, (len(traj), -1))
    forces = np.reshape(forces, (len(traj), -1))
    return coords, forces

    for atoms in traj:
        atoms.get_positions()
        

    
def lmp_to_npy(lmptrj, ener, output_dir, idx=0, atoms_kind=0):
    if os.path.exists(output_dir):
        print('Directory already exists!')
        return
    else:
        os.makedirs(output_dir)
        energies = np.array(get_ener(ener)[::2])
        ats = read(lmptrj, '::2')
        forces = np.array([at.get_forces() for at in ats])
        coords = np.array([at.get_array('positions') for at in ats])
        numbers = [at.get_array('numbers') for at in ats]
        boxes = [at.get_cell().reshape(9) for at in ats]
        if atoms_kind == 0:
            symbol_set = set(ats[0].get_chemical_symbols())
        else:
            symbol_set = atoms_kind
        #sym_dict = dict(zip(symbol_set, range(len(symbol_set))))
        #type_raw = [str(sym_dict[specie]) for specie in symbol_set]
        type_raw = [str(specie-1) for specie in numbers[0]]
        idx = str(len([l for l in os.listdir(output_dir) if 'set'in l])).zfill(3)
        os.makedirs(output_dir+'/set.'+idx)
        np.save(output_dir+'/set.'+idx+'/energy.npy', energies)
        np.save(output_dir+'/set.'+idx+'/force.npy', forces)
        np.save(output_dir+'/set.'+idx+'/coord.npy', coords)
        np.save(output_dir+'/set.'+idx+'/box.npy', boxes)
        with open(output_dir+'/type.raw', 'w') as f:
            f.write(' '.join(type_raw))
        print(len(ats))
        return 0
      
# lammpstrj to dpmdnpy
# extract some data from well-established NNP for further analyses/tests
atoms_kind = ['O', 'H', 'Pt']
lmp_to_npy(lmptrj="test.lammpstrj", ener='test.log', output_dir='system-000', atoms_kind=atoms_kind)

