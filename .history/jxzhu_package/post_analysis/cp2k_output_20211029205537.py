import os

def grep_time(output_file):
    start = "".join(os.popen("grep 'PROGRAM STARTED AT' ./output*").readlines())
    start = start.replace('\n', ' ')
    start = start.split(' ')
    # ["year", "month", "day"]
    start_date = start[-3].split("-")
    # ["hour", "minute", "second"]
    start_time = start[-2].split(":")
    

    return work_time