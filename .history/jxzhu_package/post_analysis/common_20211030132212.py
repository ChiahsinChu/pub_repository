import numpy as np


def get_1d_distribution(input_data, xmin, xmax, nbins, density=True, bound=True, errorbar=True):
    """
    get 1d distribution of input_data 
    (e.g., water density distribution @ z direction)

    Args:
        input_data: array for (x,y) pair
        xmin: min value for distribution
        xmax: max value for distribution
        nbins: number of bins for distribution
        density: if normalized to the total number of samples
    Return:
        ddd:
    """
    input_data = np.reshape(input_data, (-1, 2))
    input_data = input_data[np.argsort(input_data[:,0])]
    count = np.zeros(nbins)
    if 
