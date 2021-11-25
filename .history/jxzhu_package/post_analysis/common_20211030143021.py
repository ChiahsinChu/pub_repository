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
    bin = (xmax - xmin) / nbins 
    x = xmin + bin / 2 + np.arange(nbins) * bin
    input_data = np.reshape(input_data, (-1, 2))
    input_data = input_data[np.argsort(input_data[:,0])]
    ave_data = np.zeros(nbins)
    min_data = np.zeros(nbins)
    max_data = np.zeros(nbins)
    std_data = np.zeros(nbins)
    count_bin = 0
    tmp_data = []
    for data in input_data:
        if data[0] >= xmin + count_bin * bin and data[0] < xmin + (count_bin + 1) * bin :
            tmp_data.append(data[1])
        elif data[0] >= xmin + count_bin * bin and data[0] < xmin + (count_bin + 1) * bin :
            ave_data[count_bin] = np.mean(tmp_data)
            min_data[count_bin] = np.min(tmp_data)
            max_data[count_bin] = np.max(tmp_data)
            std_data[count_bin] = np.std(tmp_data) 
            count_bin = count_bin + 1
            tmp_data = []
            tmp_data.append(data[1])
    return ave_data, min_data, max_data, std_data
