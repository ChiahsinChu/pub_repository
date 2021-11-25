import numpy as np


def get_1d_distribution(input_data, xmin, xmax, nbins):
    """
    get 1d distribution of input_data

    Args:
        input_data: array for (x,y) pair
        xmin: min value for distribution
        xmax: max value for distribution
        nbins: number of bins for distribution
        density: if normalized to the total number of samples
    Return:
        x:
        ave_data:
        min_data:
        max_data:
        std_data:
    """
    bins = (xmax - xmin) / nbins
    #print(bins)
    x = xmin + bins / 2 + np.arange(nbins) * bins
    input_data = input_data[np.argsort(input_data[:, 0])]
    #print(input_data)
    ave_data = np.zeros(nbins)
    min_data = np.zeros(nbins)
    max_data = np.zeros(nbins)
    std_data = np.zeros(nbins)
    count_bin = int(np.floor((input_data[0][0] - xmin) / bins))
    #print(count_bin)
    tmp_data = []
    for data in input_data:
        #print(count_bin)
        #print(data)
        if data[0] < xmin:
            count_bin = 0
            continue
        if data[0] >= xmin and data[0] < xmax:
            if data[0] < xmin + count_bin * bins:
                count_bin = count_bin + 1
            elif data[0] >= xmin + count_bin * bins and data[0] < xmin + (
                    count_bin + 1) * bins:
                tmp_data.append(data[1])
            else:
                ave_data[count_bin] = np.mean(tmp_data)
                min_data[count_bin] = np.min(tmp_data)
                max_data[count_bin] = np.max(tmp_data)
                std_data[count_bin] = np.std(tmp_data)
                count_bin = count_bin + 1
                tmp_data = []
                tmp_data.append(data[1])
        else:
            break
    return x, ave_data, min_data, max_data, std_data
