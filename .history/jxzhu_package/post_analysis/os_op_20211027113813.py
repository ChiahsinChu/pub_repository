import os, sys


def get_file_list(find_dir="./", kw):
    file_list = "".join(os.popen("find ./ -name '" + kw + "' -type f ").readlines())
    s = s.replace('\n', ' ')
    s = s.split(' ')
    del s[-1]
    return file_list 


def get_dir_list(find_dir="./", kw):

    return dir_list 
    
    