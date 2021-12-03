import os
import numpy as np

def grep_time(output_file):
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
    file_start: inital time 
    file_end: final time 
    ['year','month','day','hour','minute','second']
    return time consuming for calculation in second
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
