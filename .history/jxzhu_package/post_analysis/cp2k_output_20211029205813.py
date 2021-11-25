import os

def grep_time(output_file):
    time_info = "".join(os.popen("grep 'PROGRAM STARTED AT' ./output*").readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    # ["year", "month", "day"]
    start_date = time_info[-3].split("-")
    # ["hour", "minute", "second"]
    start_time = time_info[-2].split(":")
    time_info = "".join(os.popen("grep 'PROGRAM ENDED AT' ./output*").readlines())
    time_info = time_info.replace('\n', ' ')
    time_info = time_info.split(' ')
    # ["year", "month", "day"]
    end_date = time_info[-3].split("-")
    # ["hour", "minute", "second"]
    end_time = time_info[-2].split(":")
    return start_date, start_time, end_date, end_time 