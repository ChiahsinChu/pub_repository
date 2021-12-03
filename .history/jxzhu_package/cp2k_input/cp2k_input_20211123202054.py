import json

def write_input(input_dict, output):


    return None
    
def energy(output, charge=0, multiplicity=1, uks=False, kp=False, kp_mp="2 2 1", 
           dip_cor=False, mo):
    input_dict = energy
    if charge != 0:
        input_dict["FORCE_EVAL"]["DFT"]["CHARGE"] = charge 
    if uks == True:
        input_dict["FORCE_EVAL"]["DFT"]["UKS"] = ".TRUE."
    if kp == True:
        input_dict["FORCE_EVAL"]["DFT"]["KPOINTS"] = {
        "SCHEME MONKHORST-PACK": "2 2 1",
        "SYMMETRY": "on",
        "EPS_GEO": 1.0E-8,
        "FULL_GRID": "on",
        "VERBOSE": "on",
        "PARALLEL_GROUP_SIZE": 0
        }
        input_dict["FORCE_EVAL"]["DFT"]["KPOINTS"]["SCHEME MONKHORST-PACK"] = kp_mp 
    if dip_cor == True:
        input_dict["FORCE_EVAL"]["DFT"]["SURFACE_DIPOLE_CORRECTION"] = ".TRUE."

    write_input(input_dict, output) 
    return None


# cp2k input templates
energy = {
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
        }
    },
    "GLOBAL":{
        "PROJECT": "cp2k"
    }
}
