input_dict = {
    1: 2,
    3: {
        10: 11,
        20: {
            100: 101
        },
        30: 31,
        40:{
            100: [1000, 2000, 3000],
            200: [1000, 2000, 3000]
        },
        100: 101
    }
}

default_energy = {
    "FORCE_EVAL":{
        "METHOD": "QS",
        "STRESS_TENSOR": "ANALYTICAL",
        "DFT":{
            "BASIS_SET_FILE_NAME": "BASIS_MOLOPT",
            "POTENTIAL_FILE_NAME": "GTH_POTENTIALS",
            "MGRID":{
                "CUTOFF": 1000
            },
            "QS":{
                "EPS_DEFAULT": 1.0E-13
            },
            "SCF":{
                "SCF_GUESS": "RESTART",
                "EPS_SCF": 1.0E-6,
                "MAX_SCF": 500,
                "OT":{"_":".FALSE."},
                "ADDED_MOS": 500,
                "CHOLESKY": "INVERSE",
                "SMEAR":{
                    "_": "ON",
                    "METHOD": "FERMI_DIRAC",
                    "ELECTRONIC_TEMPERATURE": 300
                },
                "DIAGONALIZATION":{
                    "ALGORITHM": "STANDARD"
                },
                "MIXING":{
                    "METHOD": "BROYDEN_MIXING",
                    "ALPHA":   0.3,
                    "BETA":    1.5,
                    "NBROYDEN":  14
                }
            },
            "XC":{
                "XC_FUNCTIONAL":{"_": "PBE"},
                "vdW_POTENTIAL":{
                    "DISPERSION_FUNCTIONAL": "PAIR_POTENTIAL",
                    "PAIR_POTENTIAL":{
                        "TYPE": "DFTD3",
                        "PARAMETER_FILE_NAME": "dftd3.dat",
                        "REFERENCE_FUNCTIONAL": "PBE"
                    }
                }
            }
        },
        "SUBSYS":{
            "CELL":{
                "A": [],
                "B": [],
                "C": []
            },
            "TOPOLOGY":{
                "COORD_FILE_FORMAT": "XYZ",
                "COORD_FILE_NAME": "./coord.xyz"
            },
            "KIND":{
                "_": ["O", "H", "Pt", "Na", "K", "Li", "C", "N", "Cl", "F"],
                "POTENTIAL": ["GTH-PBE-q6", 
                              "GTH-PBE-q1",
                              "GTH-PBE-q10",
                              "GTH-PBE-q9",
                              "GTH-PBE-q9",
                              "GTH-PBE-q3",
                              "GTH-PBE-q4",
                              "GTH-PBE-q5",
                              "GTH-PBE-q7", 
                              "GTH-PBE-q7" 
                              ],
                "BASIS_SET": ["DZVP-MOLOPT-SR-GTH", 
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-A5-Q10-323-MOL-T1-DERIVED_SET-1",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH",
                              "DZVP-MOLOPT-SR-GTH"
                              ]
            }
        },
        "PRINT":{
            "FORCES": {"_": "ON"},
            "STRESS_TENSOR": {"_": "ON"} 
        }
    },
    "GLOBAL":{
        "PROJECT": "cp2k"
    }
}


def iterdict(input_dict, out_list=["\n", "\n"], loop_idx=0):
    start_idx = len(out_list) - loop_idx - 2
    n_repeat = -1
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(-1-loop_idx, "&"+k)
            out_list.insert(-1-loop_idx, "&END "+k)
            iterdict(v, out_list, loop_idx+1)
        elif isinstance(v, list):
            n_repeat = len(v)
            #print(loop_idx)
            #print(input_dict)
            #print(out_list)
            #print(n_repeat)
            break
        else:
            v = str(v)
            if k == "_":
                out_list[start_idx] = out_list[start_idx] + " " + v
            else: 
                out_list.insert(-1-loop_idx, k+" "+v)
                #out_list.insert(-1-loop_idx, v)
    if n_repeat >= 0 :
        end_str = out_list[-1-loop_idx]
        del out_list[-1-loop_idx]
        start_str = out_list[-1-loop_idx]
        del out_list[-1-loop_idx]
        for i in range(n_repeat):
            tmp_dict = {}
            for k, v in input_dict.items():
                k=str(k)
                tmp_dict[k] = v[i]
            out_list.insert(-loop_idx, start_str)
            out_list.insert(-loop_idx, end_str)
            iterdict(tmp_dict, out_list, loop_idx)
    return out_list


input_str = iterdict(default_energy)
del input_str[0]
del input_str[-1]
print(input_str)
str = "\n".join(input_str)
print(str)
#with open("./input.inp", "w", encoding='utf-8') as f:
#    f.write(str)