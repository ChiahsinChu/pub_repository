import json

def write_input(input_dict, output_file="./input.inp"):


    return None
    
def energy(charge=0, multiplicity=1, uks=False, kp=False, kp_mp="2 2 1", 
           dip_cor=False, eden=False, mo=False, pdos=False, hartree=False, *args, **kw):
    input_dict = default_energy
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
    if mo == True:
        input_dict["FORCE_EVAL"]["DFT"]["MO_CUBES"] = {
            "ADD_LAST": "NUMERIC",
            "NHOMO": -1,
            "NLUMO": -1
        }
    if pdos == True:
        input_dict["FORCE_EVAL"]["DFT"]["PDOS"] = {
            "COMPONENTS": ".TRUE.",
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1",
            "NLUMO": -1,
            "COMMON_ITERATION_LEVELS": 0
        }
    if hartree == True:
        input_dict["FORCE_EVAL"]["DFT"]["V_HARTREE_CUBE"] = {
            "ADD_LAST": "NUMERIC",
            "STRIDE": "8 8 1"
        } 
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
    write_input(input_dict, output_file) 
    return None


def aimd(output_file="./input.inp", charge=0, multiplicity=1, uks=False,
         dip_cor=False, eden=False, mo=False, pdos=False, hartree=False, 
         restart=False, stride=1):
    input_dict = default_aimd
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
                "_": [],
                "POTENTIAL": [],
                "BASIS_SET": []
            }
        },
        "PRINT":{
            "FORCE": {"_": "ON"},
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