import os

def grep_time(output_file):
    start = "".join(os.popen("grep 'PROGRAM STARTED AT' ./output*").readlines())
    start = start.replace('\n', ' ')
    start = start.split(' ')
    # ["year", "month", "day"]
    start_date = start[-3].split("-")
    # ["hour", "minute", "second"]
    start_time = start[-2].split(":")
    end = "".join(os.popen("grep 'PROGRAM STARTED AT' ./output*").readlines())
    end = end.replace('\n', ' ')
    end = end.split(' ')
    # ["year", "month", "day"]
    end_date = end[-3].split("-")
    # ["hour", "minute", "second"]
    end_time = end[-2].split(":")
    return start_date, start_time, end_date, end_time 