import os
import numpy as np
from ase.io import write

from jxzhu_package.postprocess import format_convert


def npy_to_dpgen_fp(input_dir, input_file, type_map, output_dir="./"):
    if os.path.exists(output_dir + "iter.000000"):
        print('Directory already exist!')
    else:
        type_list = format_convert.read_type(input_dir + "type.raw")
        coord = np.load(input_dir + "set.000/coord.npy")
        coord = np.reshape(coord, (len(coord), -1, 3))
        box = np.load(input_dir + "set.000/box.npy")
        box = np.reshape(box, (len(box), -1, 3))
        for i in range(7):
            os.system("echo '0 " + str(i) + "' >> " + output_dir +
                      "record.dpgen")
        os.mkdir(output_dir + "iter.000000")
        os.mkdir(output_dir + "iter.000000/02.fp")
        for i in range(7):
            os.system("echo '0 " + str(i) + "' >> " + output_dir +
                      "iter.000000/record.dpgen")
        for i in range(len(coord)):
            os.mkdir(output_dir + "iter.000000/02.fp/task.000." +
                     str(i).rjust(6, '0'))
            atoms = format_convert.gen_atoms(type_map, type_list, coord[i])
            write(
                output_dir + "iter.000000/02.fp/task.000." +
                str(i).rjust(6, '0') + "/coord.xyz", atoms)
            # gen input file
            os.system("cp " + input_file + " " + output_dir +
                      "iter.000000/02.fp/task.000." + str(i).rjust(6, '0') +
                      "/input.inp")
            os.system("sed -i '/&CELL/a C " + str(box[i][2][0]) + " " +
                      str(box[i][2][1]) + " " + str(box[i][2][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
            os.system("sed -i '/&CELL/a B " + str(box[i][1][0]) + " " +
                      str(box[i][1][1]) + " " + str(box[i][1][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
            os.system("sed -i '/&CELL/a A " + str(box[i][0][0]) + " " +
                      str(box[i][0][1]) + " " + str(box[i][0][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
        return 0


def npy_to_dpgen_fp(input_dir, input_file, type_map, output_dir="./"):
    if os.path.exists(output_dir + "iter.000000"):
        print('Directory already exist!')
    else:
        type_list = format_convert.read_type(input_dir + "type.raw")
        coord = np.load(input_dir + "set.000/coord.npy")
        coord = np.reshape(coord, (len(coord), -1, 3))
        box = np.load(input_dir + "set.000/box.npy")
        box = np.reshape(box, (len(box), -1, 3))
        for i in range(7):
            os.system("echo '0 " + str(i) + "' >> " + output_dir +
                      "record.dpgen")
        os.mkdir(output_dir + "iter.000000")
        os.mkdir(output_dir + "iter.000000/02.fp")
        for i in range(7):
            os.system("echo '0 " + str(i) + "' >> " + output_dir +
                      "iter.000000/record.dpgen")
        for i in range(len(coord)):
            os.mkdir(output_dir + "iter.000000/02.fp/task.000." +
                     str(i).rjust(6, '0'))
            atoms = format_convert.gen_atoms(type_map, type_list, coord[i])
            write(
                output_dir + "iter.000000/02.fp/task.000." +
                str(i).rjust(6, '0') + "/coord.xyz", atoms)
            # gen input file
            os.system("cp " + input_file + " " + output_dir +
                      "iter.000000/02.fp/task.000." + str(i).rjust(6, '0') +
                      "/input.inp")
            os.system("sed -i '/&CELL/a C " + str(box[i][2][0]) + " " +
                      str(box[i][2][1]) + " " + str(box[i][2][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
            os.system("sed -i '/&CELL/a B " + str(box[i][1][0]) + " " +
                      str(box[i][1][1]) + " " + str(box[i][1][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
            os.system("sed -i '/&CELL/a A " + str(box[i][0][0]) + " " +
                      str(box[i][0][1]) + " " + str(box[i][0][2]) + " ' " +
                      output_dir + "iter.000000/02.fp/task.000." +
                      str(i).rjust(6, '0') + "/input.inp")
        return 0

