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
    return start_date, start_time, end_date, end_time 