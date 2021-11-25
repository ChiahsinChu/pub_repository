import numpy as np
from ase import io
import dpgen.generator.lib.cp2k as dpgen_cp2k


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


def make_cp2k_input(atoms, input_dict, output_dir="./", fp_params={}):
    """
    Args:
        atoms: ASE Atoms object for cell parameter and coord
        fp_params: dict for updated parameters
    """
    cell = atoms.get_cell()
    cell_a = np.array2string(cell[0])
    cell_a = cell_a[1:-1]
    cell_b = np.array2string(cell[1])
    cell_b = cell_b[1:-1]
    cell_c = np.array2string(cell[2])
    cell_c = cell_c[1:-1]

    #get update from user
    user_config = fp_params
    #get update from cell
    cell_config = {
        "FORCE_EVAL":{
            "SUBSYS":{
                "CELL":{
                    "A": cell_a,
                    "B": cell_b,
                    "C": cell_c
                }
            }
        }
            }
    dpgen_cp2k.update_dict(input_dict, user_config)
    dpgen_cp2k.update_dict(input_dict, cell_config)
    #output list
    input_str = iterdict(input_dict)
    del input_str[0]
    del input_str[-1]
    #print(input_str)
    str = "\n".join(input_str)

    io.write(output_dir+"coord.xyz", atoms)
    with open("./input.inp", "w", encoding='utf-8') as f:
        f.write(str)
    return None


def set_pp_dir(input_dict, pp_dir):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "BASIS_SET_FILE_NAME": pp_dir + "BASIS_MOLOPT",
                "POTENTIAL_FILE_NAME": pp_dir + "GTH_POTENTIALS",
                "XC":{
                    "vdW_POTENTIAL":{
                        "PAIR_POTENTIAL":{
                            "PARAMETER_FILE_NAME": pp_dir + "dftd3.dat"
                        }
                    }
                }
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_charge(input_dict, charge):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "CHARGE": charge 
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_multiplicity(input_dict, multiplicity):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "MULTIPLICITY": multiplicity 
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)
    

def set_uks(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "UKS": ".TRUE."
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_kp(input_dict, kp_mp, **kw):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "KPOINTS":{
                    "SCHEME MONKHORST-PACK": kp_mp,
                    "SYMMETRY": "ON",
                    "EPS_GEO": 1.0E-8,
                    "FULL_GRID": "ON",
                    "VERBOSE": "ON",
                    "PARALLEL_GROUP_SIZE": 0
                }
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_dip_cor(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "SURFACE_DIPOLE_CORRECTION": ".TRUE."
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_eden_cube(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "PRINT":{
                    "E_DENSITY_CUBE":{
                        "ADD_LAST": "NUMERIC",
                        "STRIDE": "8 8 1"
                    }
                } 
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_mo_cube(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "PRINT":{
                    "MO_CUBES":{
                        "ADD_LAST": "NUMERIC",
                        "NHOMO": -1,
                        "NLUMO": -1
                    }
                } 
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_pdos_cube(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "PRINT":{
                    "PDOS":{
                        "COMPONENTS": ".TRUE.",
                        "ADD_LAST": "NUMERIC",
                        "STRIDE": "8 8 1",
                        "NLUMO": -1,
                        "COMMON_ITERATION_LEVELS": 0
                    }
                }
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)


def set_hartree_cube(input_dict):
    update_d = {
        "FORCE_EVAL":{
            "DFT":{
                "PRINT":{
                    "V_HARTREE_CUBE":{
                        "ADD_LAST": "NUMERIC",
                        "STRIDE": "8 8 1"
                    }
                } 
            }
        }
    }
    dpgen_cp2k.update_dict(input_dict, update_d)
    

def set_kind(input_dict, elem, basis_set, pp):
    if (elem in input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["_"]) == False:
        # if not exist, add 
        input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["_"].append(elem)
        input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["BASIS_SET"].append(basis_set)
        input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["POTENTIAL"].append(pp)
    else:
        # if exist, update
        idx = input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["_"].index(elem)
        input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["BASIS_SET"][idx] = basis_set
        input_dict["FORCE_EVAL"]["SUBSYS"]["KIND"]["POTENTIAL"][idx] = pp 


def set_constrain(input_dict, init_idx, fin_idx, restrain=False, k_res=0.1, axis="xyz"):
    input_dict["MOTION"]["CONSTRAINT"]["FIXED_ATOMS"]["COMPONENTS_TO_FIX"].append(axis)
    input_dict["MOTION"]["CONSTRAINT"]["FIXED_ATOMS"]["LIST"].append(init_idx+".."+fin_idx)
    if restrain == True:
        input_dict["MOTION"]["CONSTRAINT"]["FIXED_ATOMS"]["RESTRAINT"].append({"K": k_res})


def set_nosiy_gamma(input_dict, init_idx, fin_idx, temp, nosiy_gamma):
    input_dict["MOTION"]["MD"]["THERMAL_REGION"]["DEFINE_REGION"]["TEMPERATURE"].append(temp)
    input_dict["MOTION"]["MD"]["THERMAL_REGION"]["DEFINE_REGION"]["NOISY_GAMMA_REGION"].append(nosiy_gamma)
    input_dict["MOTION"]["MD"]["THERMAL_REGION"]["DEFINE_REGION"]["LIST"].append(str(init_idx)+".."+str(fin_idx)) 
    return input_dict
    

def set_dp_pot(input_dict, elem, graph, elem_type):
    if_exist = False
    idx = 0
    for s in input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["ATOMS"]:
        s = s.split()
        if elem != s[0]:
            if_exist = False
        else:
            if_exist = True
            break
        idx = idx + 1
    if if_exist == False:
        # if not exist, add
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["ATOMS"].append(elem+"  "+elem)
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["POT_FILE_NAME"].append(graph)
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["ATOM_DEEPMD_TYPE"].append(elem_type)
    else:
        # if exist, update
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["ATOMS"][idx] = elem + "  " + elem
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["POT_FILE_NAME"][idx] = graph
        input_dict["FORCE_EVAL"]["MM"]["FORCEFIELD"]["NONBONDED"]["DEEPMD"]["ATOM_DEEPMD_TYPE"][idx] = elem_type 
    return input_dict


def set_lj_pot(input_dict, elem_list, **kw):
    return input_dict
    

def set_cv(input_dict, cv_type, init_idx, fin_idx):
    return input_dict
    

def energy(pp_dir=" ", charge=0, multiplicity=1, uks=False, kp=False,
           kp_mp="2 2 1", dip_cor=False, eden=False, mo=False, pdos=False, 
           hartree=False):
    input_dict = default_energy
    if pp_dir != " ":
        set_pp_dir(input_dict, pp_dir)
    if charge != 0:
        set_charge(input_dict, charge)
    if multiplicity != 1:
        set_multiplicity(input_dict, multiplicity)
    if uks == True:
        set_uks(input_dict)
    if kp == True:
        set_kp(input_dict, kp_mp)
    if dip_cor == True:
        set_dip_cor(input_dict)
    if eden == True:
        set_eden_cube(input_dict)
    if mo == True:
        set_mo_cube(input_dict)
    if pdos == True:
        set_pdos_cube(input_dict)
    if hartree == True:
        set_hartree_cube(input_dict)
    return input_dict


def geo_opt(charge=0, multiplicity=1, uks=False, kp=False, kp_mp="2 2 1", 
            dip_cor=False, eden=False, mo=False, pdos=False, hartree=False, 
            restart=False, stride=1):
    input_dict = default_geo_opt
    if charge != 0:
        input_dict["FORCE_EVAL"]["DFT"]["CHARGE"] = charge 
    if multiplicity != 1:
        input_dict["FORCE_EVAL"]["DFT"]["MULTIPLICITY"] = multiplicity 
    if uks == True:
        input_dict["FORCE_EVAL"]["DFT"]["UKS"] = ".TRUE."
    if kp == True:
        input_dict["FORCE_EVAL"]["DFT"]["KPOINTS"] = {
            "SCHEME MONKHORST-PACK": "2 2 1",
            "SYMMETRY": "ON",
            "EPS_GEO": 1.0E-8,
            "FULL_GRID": "ON",
            "VERBOSE": "ON",
            "PARALLEL_GROUP_SIZE": 0
        }
        input_dict["FORCE_EVAL"]["DFT"]["KPOINTS"]["SCHEME MONKHORST-PACK"] = kp_mp 
    if dip_cor == True:
        input_dict["FORCE_EVAL"]["DFT"]["SURFACE_DIPOLE_CORRECTION"] = ".TRUE."
    if eden == True:
        input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"]["EACH"] = {
                "GEO_OPT": stride
            }
    if mo == True:
        input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"] = {
            "ADD_LAST": "NUMERIC",
            "NHOMO": -1,
            "NLUMO": -1
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"]["EACH"] = {
                "GEO_OPT": stride
            }
    if pdos == True:
        input_dict["FORCE_EVAL"]["DFT"]["PDOS"] = {
            "COMPONENTS": ".TRUE.",
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1",
            "NLUMO": -1,
            "COMMON_ITERATION_LEVELS": 0
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["PDOS"]["EACH"] = {
                "GEO_OPT": stride
            }
    if hartree == True:
        input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        } 
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"]["EACH"] = {
                "GEO_OPT": stride
            }
    if stride != 1:
        input_dict["MOTION"]["PRINT"]["TRAJECTORY"]["EACH"] = {
            "GEO_OPT": stride
        }
        input_dict["MOTION"]["PRINT"]["VELOCITIES"]["EACH"] = {
            "GEO_OPT": stride
        }
        input_dict["MOTION"]["PRINT"]["FORCES"]["EACH"] = {
            "GEO_OPT": stride
        }
    if restart == True:
        input_dict["EXT_RESTART"] = {
            "RESTART_FILE_NAME": "cp2k-1.restart"
        }
    return input_dict 


def aimd(charge=0, multiplicity=1, uks=False, dip_cor=False, eden=False, 
         mo=False, pdos=False, hartree=False, restart=False, stride=1, aimd_type="sgcpmd"):
    if aimd_type == "sgcpmd":
        input_dict = default_aimd
        input_dict["MOTION"]["MD"] = {
            "TIMESTEP": 0.5,
            "STEPS": 30000000,
            "TEMPERATURE": 330,
            "TEMP_KIND": ".TRUE.",
            "ENSEMBLE": "LANGEVIN",
            "LANGEVIN":{
                "GAMMA": 0.001,
                "NOISY_GAMMA": 0
            },
            "THERMAL_REGION":{
                "DO_LANGEVIN_DEFAULT": ".TRUE.",
                "FORCE_RESCALING": ".TRUE.",
                "PRINT":{
                    "TEMPERATURE":{},
                    "LANGEVIN_REGIONS":{
                        "FILENAME": "__STD_OUT__"
                    }
                },
                "DEFINE_REGION":{
                    "TEMPERATURE": [],
                    "NOISY_GAMMA_REGION": [],
                    "LIST": []
                }
            } 
        }
    elif aimd_type == "bomd":
        input_dict = default_aimd
        input_dict["FORCE_EVAL"] = default_energy["FORCE_EVAL"]
        input_dict["MOTION"]["MD"] = {
            "TIMESTEP": 0.5,
            "STEPS": 30000000,
            "TEMPERATURE": 330,
            "ENSEMBLE": "NVT",
            "THERMOSTAT":{
                "REGION": "MOLECULE",
                "NOSE":{
                    "LENGTH": 3,
                    "YOSHIDA": 3,
                    "TIMECON": 1000,
                    "MTS": 2
                }
            }
        }
    else:
        raise AttributeError("Unsupported AIMD type!")

    if charge != 0:
        input_dict["FORCE_EVAL"]["DFT"]["CHARGE"] = charge 
    if multiplicity != 1:
        input_dict["FORCE_EVAL"]["DFT"]["MULTIPLICITY"] = multiplicity 
    if uks == True:
        input_dict["FORCE_EVAL"]["DFT"]["UKS"] = ".TRUE."
    if dip_cor == True:
        input_dict["FORCE_EVAL"]["DFT"]["SURFACE_DIPOLE_CORRECTION"] = ".TRUE."
    if eden == True:
        input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"]["EACH"] = {
                "MD": stride
            }
    if mo == True:
        input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"] = {
            "ADD_LAST": "NUMERIC",
            "NHOMO": -1,
            "NLUMO": -1
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"]["EACH"] = {
                "MD": stride
            }
    if pdos == True:
        input_dict["FORCE_EVAL"]["DFT"]["PDOS"] = {
            "COMPONENTS": ".TRUE.",
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1",
            "NLUMO": -1,
            "COMMON_ITERATION_LEVELS": 0
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["PDOS"]["EACH"] = {
                "MD": stride
            }
    if hartree == True:
        input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        } 
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"]["EACH"] = {
                "MD": stride
            }
    if stride != 1:
        input_dict["MOTION"]["PRINT"]["TRAJECTORY"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["VELOCITIES"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["FORCES"]["EACH"] = {
            "MD": stride
        }
    if restart == True:
        input_dict["EXT_RESTART"] = {
            "RESTART_FILE_NAME": "cp2k-1.restart"
        }
    return input_dict 


def fp_reftraj(init_idx, fin_idx, stride, charge=0, multiplicity=1, uks=False,    
               dip_cor=False, eden=False, mo=False, pdos=False, hartree=False, 
               restart=False):
    input_dict = default_aimd
    input_dict["FORCE_EVAL"] = default_energy["FORCE_EVAL"]
    input_dict["MOTION"]["MD"] = {
        "TIMESTEP": 0.5,
        "STEPS": 30000000,
        "TEMPERATURE": 330,
        "ENSEMBLE": "REFTRAJ",
        "REFTRAJ":{
            "TRAJ_FILE_NAME": "./reftraj.xyz",
            "EVAL_ENERGY_FORCES": ".TRUE.",
            "EVAL_FORCES": ".TRUE.",
            "FIRST_SNAPSHOT": init_idx,
            "LAST_SNAPSHOT": fin_idx,
            "STRIDE": stride
        }
    }
    if charge != 0:
        input_dict["FORCE_EVAL"]["DFT"]["CHARGE"] = charge 
    if multiplicity != 1:
        input_dict["FORCE_EVAL"]["DFT"]["MULTIPLICITY"] = multiplicity 
    if uks == True:
        input_dict["FORCE_EVAL"]["DFT"]["UKS"] = ".TRUE."
    if dip_cor == True:
        input_dict["FORCE_EVAL"]["DFT"]["SURFACE_DIPOLE_CORRECTION"] = ".TRUE."
    if eden == True:
        input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["E_DENSITY_CUBE"]["EACH"] = {
                "MD": stride
            }
    if mo == True:
        input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"] = {
            "ADD_LAST": "NUMERIC",
            "NHOMO": -1,
            "NLUMO": -1
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"]["EACH"] = {
                "MD": stride
            }
    if pdos == True:
        input_dict["FORCE_EVAL"]["DFT"]["PDOS"] = {
            "COMPONENTS": ".TRUE.",
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1",
            "NLUMO": -1,
            "COMMON_ITERATION_LEVELS": 0
        }
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["PDOS"]["EACH"] = {
                "MD": stride
            }
    if hartree == True:
        input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        } 
        if stride != 1:
            input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"]["EACH"] = {
                "MD": stride
            }
    if stride != 1:
        input_dict["MOTION"]["PRINT"]["TRAJECTORY"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["VELOCITIES"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["FORCES"]["EACH"] = {
            "MD": stride
        }
    if restart == True:
        input_dict["EXT_RESTART"] = {
            "RESTART_FILE_NAME": "cp2k-1.restart"
        }
    return input_dict


def dpmd(restart=False, stride=100):
    input_dict = default_dpmd
    if stride != 1:
        input_dict["MOTION"]["PRINT"]["TRAJECTORY"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["VELOCITIES"]["EACH"] = {
            "MD": stride
        }
        input_dict["MOTION"]["PRINT"]["FORCES"]["EACH"] = {
            "MD": stride
        }
    if restart == True:
        input_dict["EXT_RESTART"] = {
            "RESTART_FILE_NAME": "cp2k-1.restart"
        }
    return input_dict


def dp_reftraj(init_idx, fin_idx, stride, restart=False):
    input_dict = default_dpmd
    input_dict["MOTION"]["MD"] = {
        "TIMESTEP": 0.5,
        "STEPS": 30000000,
        "TEMPERATURE": 330,
        "ENSEMBLE": "REFTRAJ",
        "REFTRAJ":{
            "TRAJ_FILE_NAME": "./reftraj.xyz",
            "EVAL_ENERGY_FORCES": ".TRUE.",
            "EVAL_FORCES": ".TRUE.",
            "FIRST_SNAPSHOT": init_idx,
            "LAST_SNAPSHOT": fin_idx,
            "STRIDE": stride
        }
    }
    if restart == True:
        input_dict["EXT_RESTART"] = {
            "RESTART_FILE_NAME": "cp2k-1.restart"
        }
    return input_dict
    

# cp2k input templates
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

default_geo_opt = {
    "FORCE_EVAL": default_energy["FORCE_EVAL"],
    "GLOBAL":{
        "PROJECT": "cp2k",
        "RUN_TYPE": "GEO_OPT"
    },
    "MOTION":{
        "GEO_OPT":{
            "TYPE": "minimization",
            "OPTIMIZER": "BFGS",
            "MAX_ITER": 200,
            "MAX_FORCE": 4.5E-5
        },
        "CONSTRAINT": {
            "FIXED_ATOMS":{
                "COMPONENTS_TO_FIX": [],
                "LIST": [],
                "RESTRAINT": [{"K": 0.}]
            }
        },
        "PRINT":{
            "TRAJECTORY": {},
            "VELOCITIES": {},
            "FORCES": {"_":"ON"},
            "RESTART_HISTORY": {}, 
            "RESTART": {
                "BACKUP_COPIES": 3
            } 
        }
    }
}

default_aimd = {
    "FORCE_EVAL":{
        "METHOD": "QS",
        "STRESS_TENSOR": "ANALYTICAL",
        "DFT":{
            "BASIS_SET_FILE_NAME": "BASIS_MOLOPT",
            "POTENTIAL_FILE_NAME": "GTH_POTENTIALS",
            "MGRID":{
                "CUTOFF": 400
            },
            "QS":{
                "EPS_DEFAULT": 1.0E-13,
                "EXTRAPOLATION": "ASPC",
                "EXTRAPOLATION_ORDER": 0
            },
            "SCF":{
                "SCF_GUESS": "RESTART",
                "EPS_SCF": 3.0E-7,
                "MAX_SCF": 50,
                "MAX_SCF_HISTORY": 5,
                "CHOLESKY": "INVERSE_DBCSR",
                "OUTER_SCF":{
                    "EPS_SCF": 1.0E-6,
                    "MAX_SCF": 20
                },
                "OT":{
                    "MINIMIZER": "DIIS",
                    "PRECOND_SOLVER": "INVERSE_UPDATE",
                    "PRECONDITIONER": "FULL_SINGLE_INVERSE",
                    "STEPSIZE": 0.01
                },
                "PRINT":{
                    "RESTART":{
                        "EACH":{
                            "MD": 20
                        }
                    }
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
        "PROJECT": "cp2k",
        "RUN_TYPE": "MD"
    },
    "MOTION":{
        "CONSTRAINT": {
            "FIXED_ATOMS":{
                "COMPONENTS_TO_FIX": [],
                "LIST": [],
                "RESTRAINT": [{"K": 0.}]
            }
        },
        "PRINT":{
            "TRAJECTORY": {},
            "VELOCITIES": {},
            "FORCES": {"_":"ON"},
            "RESTART_HISTORY": {
                "EACH": {
                    "MD": 1000
                }
            }, 
            "RESTART": {
                "BACKUP_COPIES": 3
            } 
        }
    }
}

default_dpmd = {
    "FORCE_EVAL":{
        "METHOD": "FIST",
        "MM":{
            "FORCEFIELD":{
                "IGNORE_MISSING_CRITICAL_PARAMS": ".TRUE.",
                "CHARGE":{
                    "ATOM": ["O", "H"],
                    "CHARGE": [0., 0.]
                },
                "NONBONDED":{
                    "DEEPMD":{
                        "ATOMS": ["O O", "H H"],
                        "POT_FILE_NAME": ["graph.pb", "graph.pb"],
                        "ATOM_DEEPMD_TYPE": [0, 1],
                    }
                }
            },
            "POISSON":{
                "EWALD":{
                    "EWALD_TYPE": "none"
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
        },
    },
    "GLOBAL":{
        "PROJECT": "cp2k",
        "RUN_TYPE": "MD"
    },
    "MOTION":{
        "CONSTRAINT": {
            "FIXED_ATOMS":{
                "COMPONENTS_TO_FIX": [],
                "LIST": [],
                "RESTRAINT": [{"K": 0.}]
            }
        },
        "PRINT":{
            "TRAJECTORY": {},
            "VELOCITIES": {},
            "FORCES": {"_":"ON"},
            "RESTART_HISTORY": {
                "EACH": {
                    "MD": 50000
                }
            }, 
            "RESTART": {
                "BACKUP_COPIES": 3
            } 
        }
    }
}

default_ffmd = {

}

spce_wat = {

}

lj_param = {
    "O":{
        "epsilon": 0,
        "sigma": 0
    }
}