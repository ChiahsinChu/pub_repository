import numpy as np
import numba
from ase.io import iread
from ase import Atom, Atoms
from quippy import descriptors
from dscribe.descriptors import SOAP
from sklearn.decomposition import PCA 

from jxzhu_package.postprocess import fps, format_convert


def get_elem_idxs(type_map, type_list, elem_type):
    if (elem_type in type_map) == False :
        raise AttributeError('Element does not exist!')
    elem_idxs = []
    for idx, item in type_list:
        if elem_type == type_map[item] :
            elem_idxs.append(idx)
    return np.array(elem_idxs)


def get_atomic_number(type_map):
    new_atoms = Atoms()
    for i in type_map:
        new_atom = Atom()
        new_atom.symbol = i
        new_atoms.extend(new_atom)
    l_an = new_atoms.get_atomic_numbers()
    l_an = np.sort(l_an)

    return atomic_number
    


def gen_desc(type_map, cutoff=6.0, nmax=6, lmax=6, sigma=1.0, soap_type='dscribe'):
    if soap_type == "dscribe":
        desc = SOAP(rcut=cutoff, nmax=nmax, lmax=lmax, sigma=sigma, species=type_map, periodic=True)
        return desc 
    elif soap_type == "quip":
        atomic_number = get_atomic_number(type_map)
        desc_info = "soap cutoff=" + str(cutoff) + " l_max=" + str(lmax) + " n_max=" + str(nmax) + " atom_sigma=" + str(sigma) + " n_Z=" + str(len(type_map)) + " Z={" + atomic_number + "} "
        desc = descriptors.Descriptor(desc_info)
        return desc 
    else:
        raise AttributeError('Unsupported SOAP Type!')
    


def xyz_to_soap(xyz_file, param, desc, soap_type, if_frame=True):
    """
    generate soap from xyz_file

    Args:
        xyz_file: input file
        param: cell parameters (array for atoms.set_cell)
        desc: descriptor
        soap_type: "quip" or "dscribe"
        if_frame: soap for frame or atom
    Return:
        soap data (numpy array)
    """
    if soap_type == "quip":
        atom_data_list = []
        if if_frame == True :
            frame_data_list = []
        for atoms in iread(xyz_file):
            atoms.set_cell(param)
            atoms.set_pbc(True)
            atom_data = desc.calc(atoms)['data']
            if if_frame == True :
                frame_data = np.mean(atom_data, axis=0) 
                frame_data_list.append(frame_data)
            atom_data_list.append(atom_data)
        if if_frame == True :
            return np.array(atom_data_list), np.array(frame_data_list)
        else:
            return np.array(atom_data_list)
    elif soap_type == "dscribe":
        if if_frame == True:
            desc.average = "inner"
        else:
            desc.average = "off"
        data_list = []
        for atoms in iread(xyz_file):
            atoms.set_cell(param)
            atoms.set_pbc(True)
            data = desc.create(atoms)
            data_list.append(data)
        return np.array(data_list)
    else:
        raise AttributeError('Unsupported SOAP Type!')
        

def npy_to_soap(npy_dir, desc, soap_type, if_frame=True):
    """
    generate soap from npy file

    Args:
        npy_dir: dir of DP dataset

    """
    if soap_type == "quip":
        coords = np.load(npy_dir + "coord.npy")
        boxs = np.load(npy_dir + "box.npy")
        type_map = format_convert.read_type_map(npy_dir + "../type_map.raw")
        type_list = format_convert.read_type(npy_dir + "../type.raw")
        atom_data_list = []
        if if_frame == True :
            frame_data_list = []
        for i in range(len(coords)):
            atoms = format_convert.gen_atoms(type_map, type_list, coords[i], boxs[i])
            atom_data = desc.calc(atoms)['data']
            if if_frame == True :
                frame_data = np.mean(atom_data, axis=0) 
                frame_data_list.append(frame_data)
            atom_data_list.append(atom_data)
        if if_frame == True :
            return np.array(atom_data_list), np.array(frame_data_list)
        else:
            return np.array(atom_data_list)
    elif soap_type == "dscribe":
        coords = np.load(npy_dir + "coord.npy")
        boxs = np.load(npy_dir + "box.npy")
        type_map = format_convert.read_type_map(npy_dir + "../type_map.raw")
        type_list = format_convert.read_type(npy_dir + "../type.raw")
        if if_frame == True:
            desc.average = "inner"
        else:
            desc.average = "off"
        data_list = []
        for i in range(len(coords)):
            atoms = format_convert.gen_atoms(type_map, type_list, coords[i], boxs[i])
            data = desc.create(atoms)
            data_list.append(data)
        return np.array(data_list)
    else:
        raise AttributeError('Unsupported SOAP Type!')


def gen_elem_data(atom_data, data_type, type_map, type_list, elem_type):
    if data_type != "soap" and data_type != "force":
        raise AttributeError('Unsupported Data Type!')
    elem_idxs = get_elem_idxs(type_map, type_list, elem_type)
    data = np.load(atom_data)
    if data_type == "force":
        data = np.reshape(data, (np.shape(data)[0], -1, 3))
    elem_data = []
    for frame_data in data:
        tmp = []
        for idx in elem_idxs:
            tmp.append(frame_data[idx])
        elem_data.append(tmp)
    return np.array(elem_data)


def gen_pca_data(data):
    """
    data: 2-dimensional np array (soap array)
    """
    pca = PCA(n_components=2)
    pca.fit(data)
    pca_data = pca.transform(data)
    return pca_data

def gen_multi_pca_data(file_list):
    """
    Args: 
        file_list: dir list of `*soap_data.npy`
    """
    num_list = []
    data = np.load(file_list[0])
    data = np.reshape(data, (-1, np.shape(data)[-1]))
    num_list.append(len(data))
    for file_dir in file_list[1:]:
        _data = np.load(file_dir)
        _data = np.reshape(_data, (-1, np.shape(_data)[-1]))
        num_list.append(len(_data))
        data = np.concatenate((data, _data), axis=0)
    pca_data = gen_pca_data(data) 
    return pca_data, data, np.array(num_list)
    

def check_pca(data, p=0.99):
    """
    check how the PCA representative 

    Args:
        data: 2-dimensional np array (soap array)
        p: the ratio of PCA
    """
    pca = PCA(n_components=p)
    pca.fit(data)
    print(pca.explained_variance_ratio_)
    return 0


def gen_pes_slope(soap_data, energy_data):
    soap_data = np.array(soap_data)
    energy_data = np.array(energy_data)
    pes_slope = _gen_pes_slope(soap_data, energy_data)
    return pes_slope


@numba.njit
def _gen_pes_slope(soap_data, energy_data):
    pes_slope = np.zeros(len(soap_data))
    inner_diss_matrix = fps.get_inner_diss_matrix(soap_data)
    for idx_a, inner_diss in enumerate(inner_diss_matrix):
        inner_diss[idx_a] = np.max(inner_diss) + 1
        idx_b = np.argmin(inner_diss)
        pes_slope[idx_a] = (energy_data[idx_a] - energy_data[idx_b]) / np.min(inner_diss) 
    return pes_slope


def gen_afs_slope(atom_soap_data, force_data):
    atom_soap_data = np.array(atom_soap_data)
    force_data = np.reshape(force_data, (np.shape(force_data)[0], -1, 3))
    afs_slope = _gen_afs_slope(atom_soap_data, force_data) 
    return afs_slope


@numba.njit
def _gen_afs_slope(atom_soap_data, force_data):
    afs_slope = np.zeros(len(atom_soap_data))
    delta_force = np.zeros(np.shape(force_data[1]))
    inner_diss_matrix = fps.get_inner_diss_matrix(atom_soap_data)
    for idx_a, inner_diss in enumerate(inner_diss_matrix):
        inner_diss[idx_a] = np.max(inner_diss) + 1
        idx_b = np.argmin(inner_diss)
        for i in range(len(delta_force)):
            delta_force[i] = np.linalg.norm(force_data[idx_a][i] - force_data[idx_b][i])
        afs_slope[idx_a] = np.max(delta_force) / np.min(inner_diss) 
    return afs_slope



    