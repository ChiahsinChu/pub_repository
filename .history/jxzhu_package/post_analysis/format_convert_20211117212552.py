import numpy as np
from ase import io, Atom, Atoms
import glob

def get_atoms_data(npy_file):
    """
    get coord/frc/box from npy file
    """
    data = np.load(npy_file)
    data = np.reshape(data, (np.shape(data), -1, 3))
    return data

def read_type_map(type_map_file):
    with open(type_map_file, 'r') as f:
        type_map = f.read()
        type_map = type_map.replace('\n', ' ')
        type_map = type_map.split(' ')
        del type_map[-1]
    return type_map


def read_type(type_file):
    with open(type_file) as f:
        context = f.read()
    a = np.array(context.split())
    b = a.astype(int)
    return b


def gen_atoms(type_map, type_list, coord, box):
    new_atoms = Atoms()
    coord = np.reshape(coord, (-1, 3))
    box = np.reshape(box, (-1, 3))
    for i in range(len(coord)):
        new_atom = Atom()
        new_atom.symbol = type_map[type_list[i]]
        new_atom.position = coord[i]
        new_atoms.extend(new_atom)
    new_atoms.set_cell(box)
    new_atoms.set_pbc(True)
    return new_atoms

def struc_check(data_dir, output_dir, output_file="reftraj.xyz"):
    """
    convert npy dataset to xyz file

    Arg:
        data_dir: dir of data for dp training
        output_dir: dir to store the output xyz files

    """
    l_set = glob.glob(data_dir + "/set.*")
    for l in l_set:
        type_map = read_type_map(data_dir +  "/type_map.raw")
        type_list = read_type(data_dir +  "/type.raw")
        coords = np.load(l +  "/coord.npy")
        boxs = np.load(l +  "/box.npy")
        for idx in range(len(coords)):
            atoms = gen_atoms(type_map, type_list, coords[idx], boxs[idx])
            io.write(output_dir +  "/" + output_file, atoms, append=True)
    return 0
