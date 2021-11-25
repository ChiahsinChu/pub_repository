import numpy as np
from ase import io, Atom, Atoms
import glob, time, os, sys

from jxzhu_package.logger import Logger

au2eV = 2.72113838565563E+01
au2A = 5.29177208590000E-01


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
    type_map = type_map.strip()
    type_map = type_map.split('\n')
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


def npy_to_xyz(data_dir, output_dir, output_file="reftraj.xyz"):
    """
    convert npy dataset to xyz file

    Arg:
        data_dir: dir of data for dp training
        output_dir: dir to store the output xyz files

    """
    output_dir = output_dir.strip('\')
    sys.stdout = Logger('./job.log')
    print(os.getcwd())
    start = time.process_time()

    # print input parameters
    print("npy data directory: ", data_dir)
    print("output directory: ", output_dir)
    print("output file: ", output_dir +  "/" + output_file )

    l_set = glob.glob(data_dir + "/set.*")
    type_map = read_type_map(data_dir +  "/type_map.raw")
    type_list = read_type(data_dir +  "/type.raw")
    for l in l_set:
        coords = np.load(l +  "/coord.npy")
        boxs = np.load(l +  "/box.npy")
        for idx in range(len(coords)):
            atoms = gen_atoms(type_map, type_list, coords[idx], boxs[idx])
            io.write(output_dir +  "/" + output_file, atoms, append=True)

    fmt = "\nWork Completed! Used Time: {:.3f} seconds"
    print(fmt.format(time.process_time() - start))

    return 0


def get_xyz_energy(traj):
    energy = []
    for atoms in traj:
        energy.append(atoms.info['E'])
    energy = np.array(energy) * au2eV
    return energy


def get_xyz_coord(traj):
    coord = []
    for atoms in traj:
        coord.append(atoms.get_positions())
    coord = np.reshape(coord, (len(traj), -1))
    return coord


def get_xyz_force(frcs):
    force = []
    for atoms in frcs:
        force.append(atoms.get_positions())
    force = np.reshape(force, (len(frcs), -1)) * au2eV / au2A
    return force


def get_xyz_box(traj, cell_param):
    atoms = traj[0]
    atoms.set_cell(cell_param)
    cell_param = atoms.get_cell()
    box = [cell_param] * len(traj)
    box = np.reshape(box, (len(traj), -1))
    return box


def get_xyz_type(atoms, type_map):
    type_list = atoms.get_chemical_symbols()
    for i in range(len(type_map)):
        type_list = [str(i) if j == type_map[i] else j for j in type_list]
        #type_list=type_list.replace(type_map[i],str(i))
    return type_list

def xyz_to_npy(pos_file, frc_file, cell_param, type_map, output_dir="./system"):
    sys.stdout = Logger('./job.log')
    print(os.getcwd())
    start = time.process_time()

    # print input parameters
    print("xyz file for coord: ", pos_file)
    print("xyz file for force: ", frc_file)
    print("cell param: ", cell_param)
    print("type map: ", type_map)
    print("output directory: ", output_dir)

    if os.path.exists(output_dir):
        print('Directory already exist!')
    else:
        traj = io.read(pos_file, index=":")
        frcs = io.read(frc_file, index=":")
        energy = get_xyz_energy(traj)
        coord = get_xyz_coord(traj)
        force = get_xyz_force(frcs)
        box = get_xyz_box(traj, cell_param)
        type_list = get_xyz_type(traj[0], type_map)

        os.mkdir(output_dir)
        os.mkdir(output_dir+"/set.000")
        with open(output_dir+'/type_map.raw', 'w') as f:
            f.write('\n'.join(type_map))
        with open(output_dir+'/type.raw', 'w') as f:
            f.write(' '.join(type_list))

        np.save(output_dir+"/set.000/energy.npy", energy)
        np.save(output_dir+"/set.000/coord.npy", coord)
        np.save(output_dir+"/set.000/force.npy", force)
        np.save(output_dir+"/set.000/box.npy", box)

    print("Number of Frame:", len(traj))
    fmt = "\nWork Completed! Used Time: {:.3f} seconds"
    print(fmt.format(time.process_time() - start))


