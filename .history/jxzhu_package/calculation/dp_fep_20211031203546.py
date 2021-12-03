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
    data_dir: directory of model deviation
    
    return
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