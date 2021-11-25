import os, sys, time
from random import shuffle
import numpy as np
import numba
from ase.io import iread, read, write


AU_TO_ANG = 5.29177208590000E-01
AU_TO_EV = 2.72113838565563E+01
AU_TO_EV_EVERY_ANG = AU_TO_EV/AU_TO_ANG


def get_param(data_dir="./"):
    """

    Arg:
        data_dir: directory of model deviation

    Return:
        lambdas: list of lambda
        temps: list of temp
        configs: list of config index
        nnps: list of nnp index
    """
    # get list of param
    #ls ../ -F | grep '/$'

    temps = []
    os.system("ls " + data_dir + " -F | grep '/$' > " + data_dir + "/temp.dat")
    with open(data_dir + "/temp.dat", "r") as f:
        for line in f.readlines():
            #print(line[:-2])
            temps.append(line[:-2])
    #print(temp)

    configs = []
    os.system("ls " + data_dir + "/" + temps[0] + " -F | grep '/$' > " +
              data_dir + "/config.dat")
    with open(data_dir + "/config.dat", "r") as f:
        for line in f.readlines():
            #print(line[:-2])
            configs.append(line[:-2])
    #print(configs)

    nnps = []
    os.system("ls " + data_dir + "/" + temps[0] + "/" + configs[0] +
              " -F | grep '/$' > " + data_dir + "/nnp.dat")
    with open(data_dir + "/nnp.dat", "r") as f:
        for line in f.readlines():
            #print(line[:-2])
            nnps.append(line[:-2])
    ## remove the name of sampling dir
    del nnps[-1]
    os.system("rm " + data_dir + "/*dat")
    return temps, configs, nnps


@numba.njit
def get_frc_tot(frc, unit_trans=0):
    """
    frc_x/y/z: force
    
    return
    frc: total force in eV/A
    """
    frc_tot = np.linalg.norm(frc)
    if unit_trans == 1:
        frc_tot = frc_tot * AU_TO_EV_EVERY_ANG
    return frc_tot


@numba.njit
def get_frc_devi(ev_data):
    """
    Arg:
    ev_data: list of atoms (frc) [eV/A] from various nnp (n_nnp, n_frame, n_atom, 3)
    
    Return:
        devi_f: (4* nframe * natom) std between all frcs from NNP for each atom
        max_devi_f: (4 * nframe) self-explanatory 
        min_devi_f: (4 * nframe) self-explanatory 
        avg_devi_f: (4 * nframe) self-explanatory 
        id_accurate: index of accurate
        id_candidate: index of candidate 
        id_candidate: index of candidate 
    """
    # model deviation for each frame (x, y, z, tot)
    devi_f = np.zeros((4, len(ev_data[0]), len(ev_data[0][0])))
    max_devi_f = np.zeros((4, len(ev_data[0])))
    min_devi_f = np.zeros((4, len(ev_data[0])))
    avg_devi_f = np.zeros((4, len(ev_data[0])))
    # go through all frames
    for i in range(len(ev_data[0])):
        # std_frc for each atom (x, y, z, tot)
        std_frc = np.zeros((4, len(ev_data[0][0])))
        # go through all atoms
        for j in range(len(ev_data[0][0])):
            # frc for each atom at each nnp (x, y, z, tot)
            frc = np.zeros((3, len(ev_data)))
            # go through all nnps
            for k in range(len(ev_data)):
                frc[0][k] = ev_data[k][i][j][0]
                frc[1][k] = ev_data[k][i][j][1]
                frc[2][k] = ev_data[k][i][j][2]
                #frc[3][k] = get_frc_tot(ev_data[k][i][j])
            for k in range(3):
                std_frc[k][j] = np.std(frc[k])
        for j in range(4):
            devi_f[j][i] = std_frc[j]
            max_devi_f[j][i] = np.max(std_frc[j])
            min_devi_f[j][i] = np.min(std_frc[j])
            avg_devi_f[j][i] = np.mean(std_frc[j])

    return devi_f, max_devi_f, min_devi_f, avg_devi_f


@numba.njit
def get_frc_devi(ev_data):
    """
    Arg:
    ev_data: list of atoms (frc) [eV/A] from various nnp (n_nnp, n_frame, n_atom, 3)
    
    Return:
        devi_f: (4* nframe * natom) std between all frcs from NNP for each atom
        max_devi_f: (4 * nframe) self-explanatory 
        min_devi_f: (4 * nframe) self-explanatory 
        avg_devi_f: (4 * nframe) self-explanatory 
        id_accurate: index of accurate
        id_candidate: index of candidate 
        id_candidate: index of candidate 
    """
    # std @ x/y/z direction
    devi_f_xyz = np.std(ev_data, axis=0)
    devi_f = np.zeros((4, len(ev_data[0]), len(ev_data[0][0])))
    max_devi_f = np.zeros((4, len(ev_data[0])))
    min_devi_f = np.zeros((4, len(ev_data[0])))
    avg_devi_f = np.zeros((4, len(ev_data[0]))) 

    for atoms_idx, atoms in enumerate(devi_f_xyz):
        for atom_idx, atom in enumerate(atoms):

    # go through all frames
    for i in range(len(ev_data[0])):
        # std_frc for each atom (x, y, z, tot)
        std_frc = np.zeros((4, len(ev_data[0][0])))
        # go through all atoms
        for j in range(len(ev_data[0][0])):
            # frc for each atom at each nnp (x, y, z, tot)
            frc = np.zeros((3, len(ev_data)))
            # go through all nnps
            for k in range(len(ev_data)):
                frc[0][k] = ev_data[k][i][j][0]
                frc[1][k] = ev_data[k][i][j][1]
                frc[2][k] = ev_data[k][i][j][2]
                #frc[3][k] = get_frc_tot(ev_data[k][i][j])
            for k in range(3):
                std_frc[k][j] = np.std(frc[k])
        for j in range(4):
            devi_f[j][i] = std_frc[j]
            max_devi_f[j][i] = np.max(std_frc[j])
            min_devi_f[j][i] = np.min(std_frc[j])
            avg_devi_f[j][i] = np.mean(std_frc[j])

    return devi_f, max_devi_f, min_devi_f, avg_devi_f



def get_candidate(id_candidate,
                  n_configs,
                  input_dir="./",
                  output_data="./out.xyz"):
    shuffle(id_candidate)
    os.system("rm " + output_data)
    for i in range(n_configs):
        data = input_dir + str(id_candidate[i][0]) + "/" + str(
            id_candidate[i][1]).rjust(3, '0') + "/sampling/dpmd-pos-1.xyz"
        atoms = read(data, index=id_candidate[i][2])
        write(output_data, atoms, append=True)
    return 0