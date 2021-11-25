import os, sys, time
from random import shuffle
import numpy as np
import numba
from ase.io import iread, read, write

AU_TO_ANG = 5.29177208590000E-01
AU_TO_EV = 2.72113838565563E+01
AU_TO_EV_EVERY_ANG = AU_TO_EV/AU_TO_ANG

