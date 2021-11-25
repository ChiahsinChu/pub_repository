import numpy as np
import numba
from ase.io import iread
from quippy import descriptors
from dscribe.descriptors import SOAP
from sklearn.decomposition import PCA 

from jxzhu_package.postprocess import fps


def _gen_quip_soap(atoms, desc):
    """
    generate soap for a structure (`atoms`) given descriptor (`desc`) 

    Args:
        atoms: Atoms object in ASE
        desc: Descriptor object in quippy.descriptor
    """
    data = desc.calc(atoms)['data']
    return data


def gen_quip_soap(xyz_file, param, desc, if_frame=True):
    """
    generate frame soap (average from atom soap)
    for a xyz trajectory file given descriptor (`desc`)
    by quip 
    """
    atom_data_list = []
    if if_frame == True :
        frame_data_list = []
    for atoms in iread(xyz_file):
        atoms.set_cell(param)
        atoms.set_pbc(True)
        atom_data = _gen_quip_soap(atoms, desc)
        if if_frame == True :
            frame_data = np.mean(atom_data, axis=0) 
            frame_data_list.append(frame_data)
        atom_data_list.append(atom_data)
    if if_frame == True :
        return np.array(atom_data_list), np.array(frame_data_list)
    else:
        return np.array(atom_data_list)


def gen_dscribe_soap(xyz_file, param, desc):
    """
    generate soap for a xyz trajectory file given descriptor (`desc`) 
    by dscribe
    self.soap_descriptor = SOAP(
            species=self.species,
            rcut=rcut, nmax=nmax, lmax=lmax, sigma=sigma,
            average="inner", crossover=True,
            periodic=periodic
        )
    """
    data_list = []
    for atoms in iread(xyz_file):
        atoms.set_cell(param)
        atoms.set_pbc(True)
        data = desc.create(atoms)
        data_list.append(data)
    return np.array(data_list)


def gen_pca_data(data):
    """
    data: 2-dimensional np array (soap array)
    """
    pca = PCA(n_components=2)
    pca.fit(data)
    pca_data = pca.transform(data)
    #plt.scatter(pca_data[:, 0], pca_data[:, 1],marker='o')
    #plt.show()
    return pca_data


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


@numba.njit
def _gen_pes_slope(soap_data, energy_data):
    pes_slope = np.zeros(len(soap_data))
    inner_diss_matrix = fps.get_inner_diss_matrix(soap_data)
    for idx_a, inner_diss in enumerate(inner_diss_matrix):
        inner_diss[idx_a] = np.max(inner_diss) + 1
        idx_b = np.argmin(inner_diss)
        pes_slope[idx_a] = (energy_data[idx_a] - energy_data[idx_b]) / np.min(inner_diss) 
    return pes_slope


def gen_pes_slope(soap_data, energy_data):
    soap_data = np.array(soap_data)
    energy_data = np.array(energy_data)
    pes_slope = _gen_pes_slope(soap_data, energy_data)
    return pes_slope

@numba.njit
def _gen_afs_slope(soap_data, force_data):
    pes_slope = np.zeros(len(soap_data))
    inner_diss_matrix = fps.get_inner_diss_matrix(soap_data)
    for idx_a, inner_diss in enumerate(inner_diss_matrix):
        inner_diss[idx_a] = np.max(inner_diss) + 1
        idx_b = np.argmin(inner_diss)
        pes_slope[idx_a] = (energy_data[idx_a] - energy_data[idx_b]) / np.min(inner_diss) 
    return afs_slope


def gen_afs_slope(soap_data, force_data):
    soap_data = np.array(soap_data)
    force_data = np.array(force_data)
    afs_slope = _gen_afs_slope(soap_data, force_data) 
    return afs_slope
    