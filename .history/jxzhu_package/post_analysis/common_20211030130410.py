import numpy as np


def get_1d_distribution(input_data, xmin, xmax, nbins, density=True):
    """
    get 1d distribution of input_data 
    (e.g., water density distribution @ z direction)

    Args:
        input_data: 1-D list or numpy array
        xmin: min value for distribution
        xmax: max value for distribution
        nbins: number of bins for distribution
    """