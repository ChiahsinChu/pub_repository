from ase import Atoms, io, build
import numpy as np
import os


def get_energies(energy_file="energy.log"):
    energies = np.loadtxt(energy_file)[:, 2]
    return energies


def read_dump(dump_file="./dump.lammpstrj"):
    traj = io.read(dump_file, index=":")
    coords = []
    forces = []
    for atoms in traj:
        forces.append(atoms.get_forces())
        coords.append(atoms.get_positions())
    coords = np.reshape(coords, (len(traj), -1))
    forces = np.reshape(forces, (len(traj), -1))
    type_list = atoms.get_array('numbers')
    type_list = np.array(type_list) - 1
    cell_param = atoms.get_cell()
    return coords, forces, type_list, cell_param

    


