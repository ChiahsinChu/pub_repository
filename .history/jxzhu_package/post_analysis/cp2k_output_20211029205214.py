import os

def grep_time(output_file):
    start = "".join(os.popen("grep 'PROGRAM STARTED AT' ./output*").readlines())
    start = start.replace('\n', ' ')
    start = start.split(' ')
    start_time = start[-2]
    start_date = start[-3]
    
    return work_time