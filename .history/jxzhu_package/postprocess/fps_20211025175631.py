import numpy as np
import numba

@numba.njit
def get_inner_diss_matrix(soap_add):
    """
    generate distance matrix (based on soap) between the new structures 

    return:
        inner_diss_matrix[i][j]: distance between i-th and j-th structure in soap_add
    """
    inner_diss_matrix = np.zeros((len(soap_add),len(soap_add))) 
    for i, item in enumerate(soap_add):
        for j in range(i+1, len(soap_add)):
            inner_diss_matrix[i][j] = np.linalg.norm(item - soap_add[j])
            inner_diss_matrix[j][i] = inner_diss_matrix[i][j]
        inner_diss_matrix[i][i] = 0
    return inner_diss_matrix

@numba.njit
def get_inter_diss_matrix(soap_add, soap_ori):
    """
    generate distance matrix (based on soap) between the new structures and the original structrues

    return:
        inter_diss_matrix[i][j]: distance between i-th structure in soap_add and j-th structure in soap_add
    """
    inter_diss_matrix = np.zeros((len(soap_add),len(soap_ori))) 
    for i, i_item in enumerate(soap_add):
        for j, j_item in enumerate(soap_ori): 
            inter_diss_matrix[i][j] = np.linalg.norm(i_item - j_item)
    return inter_diss_matrix


def soap_pick_idx(inner_diss_matrix, inter_diss_matrix, fp_task_max):
    """
    find (fp_task_max) structures with farthest distance to other stuctures (both original and added structure)

    Args:
        inner_diss_matrix: (len(soap_add),len(soap_add))
        inter_diss_matrix: (len(soap_add),len(soap_ori))
        fp_task_max: number of selected structures

    return:
        top_k_idx: list of index of selected structures 
    """
    full_idx = np.arange(len(inner_diss_matrix))
    #print(np.shape(inner_diss_matrix))
    #print("full_idx is ", full_idx)
    _top_k_idx = np.zeros(fp_task_max, dtype=np.int32)
    for i in range(fp_task_max):
        diss = np.min(inter_diss_matrix, axis=1)
        _top_k_idx[i] = np.argmax(diss)
        tmp_diss = np.delete(inner_diss_matrix[np.argmax(diss)],np.argmax(diss))
        inner_diss_matrix = np.delete(inner_diss_matrix, np.argmax(diss), axis=0)
        inner_diss_matrix = np.delete(inner_diss_matrix, np.argmax(diss), axis=1)
        inter_diss_matrix = np.delete(inter_diss_matrix, np.argmax(diss), axis=0)
        inter_diss_matrix = np.transpose(inter_diss_matrix)
        inter_diss_matrix = np.concatenate((inter_diss_matrix,np.array([tmp_diss])), axis=0)
        inter_diss_matrix = np.transpose(inter_diss_matrix) 
    top_k_idx = np.zeros(fp_task_max, dtype=np.int32)
    print("_top_k_idx is ", _top_k_idx)
    for i, item in enumerate(_top_k_idx):
        top_k_idx[i] = full_idx[item]
        full_idx = np.delete(full_idx, item, axis=0)
    print("top_k_idx is ", top_k_idx)
    return top_k_idx 
