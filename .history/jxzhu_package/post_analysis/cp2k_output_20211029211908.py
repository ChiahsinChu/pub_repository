import os

def grep_time(output_file):
    time_info = "".join(os.popen("grep 'PROGRAM STARTED AT' " + output_file).readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    # ["year", "month", "day"]
    data = time_info[-3].split("-")
    start_date = [float(data[0]), float(data[1]), float(data[2])]
    # ["hour", "minute", "second"]
    data = time_info[-2].split(":")
    start_time = [float(data[0]), float(data[1]), float(data[2])]
    time_info = "".join(os.popen("grep 'PROGRAM ENDED AT' " + output_file).readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    data = time_info[-3].split("-")
    end_date = [float(data[0]), float(data[1]), float(data[2])]
    data = time_info[-2].split(":") 
    end_time = [float(data[0]), float(data[1]), float(data[2])]
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
