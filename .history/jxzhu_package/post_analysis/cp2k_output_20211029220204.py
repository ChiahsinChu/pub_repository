import os, re
import numpy as np


def cp2k_worktime(output_file="./output*"):
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
    """
    # ["year", "month", "day"]
    data = time_info[-3].split("-")
    for item in data:
        start.append(float(item))
    """
    # ["hour", "minute", "second"]
    data = time_info[-2].split(":")
    for item in data:
        start.append(float(item))

    time_info = "".join(os.popen("grep 'PROGRAM ENDED AT' " + output_file).readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    end = []
    """
    data = time_info[-3].split("-")
    for item in data:
        end.append(float(item))
    """
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

def grep_force(output_file):
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
    return force_lines
    

    
