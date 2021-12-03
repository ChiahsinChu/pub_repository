import os, sys


def get_file_list(find_dir="./", kw):
    file_list = "".join(os.popen("find " + find_dir + " -name '" + kw + "' -type f ").readlines())
    file_list = file_list.replace('\n', ' ')
    file_list = file_list.split(' ')
    del file_list[-1]
    return file_list 


def get_dir_list(find_dir="./", kw):

    return dir_list 
    
    