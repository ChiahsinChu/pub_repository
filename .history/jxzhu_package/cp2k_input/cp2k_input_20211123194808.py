import json


# cp2k input templates
energy = {
    "FORCE_EVAL":{
        "DFT":{
            "BASIS_SET_FILE_NAME": "/data/jxzhu/basis/BASIS_MOLOPT",
            "POTENTIAL_FILE_NAME": "/data/jxzhu/basis/GTH_POTENTIALS",
            "MGRID":{
                "CUTOFF": 1000
            },
            "QS":{
                "EPS_DEFAULT": 1.0E-13
            },
            "SCF":{
                "SCF_GUESS": "ATOMIC",
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
                                "PARAMETER_FILE_NAME": "/data/jxzhu/basis/dftd3.dat",
                                "REFERENCE_FUNCTIONAL": "PBE"
                        }
                    }
            }
        },
        "SUBSYS":{
            "KIND":{
                "_": ["O", "H","Pt"],
                "POTENTIAL": ["GTH-PBE-q6", "GTH-PBE-q1","GTH-PBE-q10"],
                "BASIS_SET": ["DZVP-MOLOPT-SR-GTH", "DZVP-MOLOPT-SR-GTH","DZVP-A5-Q10-323-MOL-T1-DERIVED_SET-1"]
            }
        }
    }
}
