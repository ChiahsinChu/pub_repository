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
        x: centers of bins
        ave_data: ave value of all data within every bin
        min_data: min value of all data within every bin
        max_data: min value of all data within every bin
        std_data: std value of all data within every bin
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


def vec_cos(a,b):
    """
    Takes 2 vectors a, b and returns the cosine
    """
    output = np.dot(a,b) / (np.linalg.norm(a) * np.linalg.norm(b))
    return output
    
    
def check_cell(atoms):
    """
    Arg:
        ASE Atoms object for check
    """
    cell = atoms.get_cell()
    a = cell[0]
    b = cell[1]
    c = cell[2]
    normal = np.cross(a,b)
    if np.abs(vec_cos(normal,c)) != 1:
        raise AttributeError('vector c must perpendicular to plane formed by a and b')
    else:
        return None


def cell_cross_area(atoms):
    """
    Calculate the cross section area of the cell
    """
    check_cell(atoms)
    area = 



    
