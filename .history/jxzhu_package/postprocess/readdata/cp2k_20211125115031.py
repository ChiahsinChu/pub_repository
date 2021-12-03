import os, re
import numpy as np
from ase import io


AU_TO_ANG = 5.29177208590000E-01
AU_TO_EV = 2.72113838565563E+01
AU_TO_EV_EVERY_ANG = AU_TO_EV/AU_TO_ANG


def get_worktime(output_file="./output*"):
    start, end = grep_time(output_file)
    worktime = time_gap(start, end)
    return worktime


def grep_time(output_file="./output*"):
    """
    grep the time info from cp2k output file

    Return:
        float list of time ["hour", "minute", "second"]
    """
    time_info = "".join(os.popen("grep 'PROGRAM STARTED AT' " + output_file).readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    start = []
    # ["hour", "minute", "second"]
    data = time_info[-2].split(":")
    for item in data:
        start.append(float(item))

    time_info = "".join(os.popen("grep 'PROGRAM ENDED AT' " + output_file).readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    end = []
    data = time_info[-2].split(":")
    for item in data:
        end.append(float(item))
    return start, end 


def time_gap(start, end):
    """
    Args:
        start: float list for inital time 
        end: float list for final time 
        in ['hour','minute','second']
    Return:
        time consuming for calculation 
        in second
    """
    t_i = np.array(start)
    t_f = np.array(end)
    t = t_f - t_i
    # second
    if t[-1] < 0:
        t[-1] = t[-1] + 60
        t[-2] = t[-2] - 1
    # minute
    if t[-2] < 0:
        t[-2] = t[-2] + 60
        t[-3] = t[-3] - 1
    # hour
    if t[-3] < 0:
        t[-3] = t[-3] + 24
    worktime = t[-1] + t[-2]*60 + t[-3]*60*60
    return worktime


def get_force(output_file="./output*"):
    """
    get atomic force from cp2k output

    Return:
        force numpy array (n_atom, 3)
    """
    with open (output_file,"r") as f:
        lines = f.readlines()
    force_start_pattern = re.compile(r' ATOMIC FORCES in')
    force_flag=False
    force_end_pattern = re.compile(r' SUM OF ATOMIC FORCES')
    force_lines= []
    for line in lines:
        line = line.strip('\n')
        if force_start_pattern.match(line):
            force_flag=True
        if force_end_pattern.match(line):
            assert force_flag is True, (force_flag,'there may be errors in this file ')
            force_flag=False
        if force_flag is True:
                force_lines.append(line)
    #force_lines = force_lines[:3]
    forces_list = []
    for line in force_lines[3:]:
        line_list = line.split()
        forces_list.append([float(line_list[3]) * AU_TO_EV_EVERY_ANG,
            float(line_list[4])*AU_TO_EV_EVERY_ANG,
            float(line_list[5])*AU_TO_EV_EVERY_ANG])
    return np.array(forces_list)
    

def get_energy(output_file="./output*"):
    data = "".join(os.popen("grep 'Total energy:' " + output_file).readlines())
    data = data.replace('\n', ' ')
    data = data.split(' ')
    return float(data[-2]) * AU_TO_EV


def get_forces(frc_file="./cp2k-frc-1.xyz"):
    frcs = read(frc_file, index=":")
    force = []
    for atoms in frcs:
        force.append(atoms.get_positions())
    force = np.reshape(force, (len(frcs), -1)) * AU_TO_EV_EVERY_ANG
    return force
