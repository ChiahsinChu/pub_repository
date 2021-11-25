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
    # ["year", "month", "day"]
    end_date = time_info[-3].split("-")
    # ["hour", "minute", "second"]
    end_time = time_info[-2].split(":")
    return start_date, start_time, end_date, end_time 