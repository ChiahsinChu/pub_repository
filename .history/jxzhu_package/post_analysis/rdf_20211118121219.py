import numpy as np
import numba
from ase.io import iread


def get_dis(a1, a2, param, d_max, n_layer):
    """
    get the distance between atom1 and atom2

    Args:
        a1: position of atom1 ([x1, y1, z1], numpy array)
        a2: position of atom2 ([x2, y2, z2], numpy array)
        param: cell parameter ([a, b, c, alpha, beta, gamma])
        **Orthognoal or Hexagnol Only!!!**
        d_max: max distance considered in analysis
        n_layer: 1 or 2, consider one cell or other 8 neighbouring cells
    Return:
        d: a distance value (n_layer=1) or a distance array (n_layer=2)
    """
    # dtype convertion
    a1 = np.array(a1, dtype=float)
    a2 = np.array(a2, dtype=float)
    param = np.array(param, dtype=float)
    d_max = np.float64(d_max)
    n_layer = np.int32(n_layer)

    # orthogonal
    if param[3] == 90. and param[4] == 90. and param[5] == 90.:
        # central cell only
        if n_layer == 1:
            # check if the cutoff exceeds the cell boundary
            if d_max > param[0] / 2. or d_max > param[1] / 2.:
                raise AttributeError('Cutoff Crosses the Cell!')
            d, a2 = get_1st_dis(a1, a2, param)
        elif n_layer == 2:
            if d_max > param[0] / 2. * 3. or d_max > param[1] / 2. * 3.:
                raise AttributeError('Cutoff Crosses the Cell!')
            d0, a2 = get_1st_dis(a1, a2, param)
            d = get_2nd_dis(a1, a2, param)
            d[1][1] = d0
        else:
            raise AttributeError('Unspported n_layer!')
    # hexagonal
    elif param[3] == 90. and param[4] == 90. and param[0] == param[1] and (
            param[5] == 120. or param[5] == 60.):
        # central cell only
        if n_layer == 1:
            # check if the cutoff exceeds the cell boundary
            if d_max > param[0] * np.sqrt(3) / 4.:
                raise AttributeError('Cutoff Crosses the Cell!')
            d, a2 = get_1st_dis(a1, a2, param)
        elif n_layer == 2:
            if d_max > param[0] * np.sqrt(3) / 4. * 3.:
                raise AttributeError('Cutoff Crosses the Cell!')
            d0, a2 = get_1st_dis(a1, a2, param)
            d = get_2nd_dis(a1, a2, param)
            d[1][1] = d0
        else:
            raise AttributeError('Unspported n_layer!')
    else:
        raise AttributeError('Orthogonal or Hexagnoal Cell Only!')
    return d


@numba.njit
def get_1st_dis(a1, a2, param):
    """
    get the nearest distance at PBC

    Args:
        a1: atom 1 coordination [x, y, z]
        a2: atom 2 coordination [x, y, z] 
        param: cell parameter [a, b, c, alpha, beta, gamma]
        ** only for orthogonal or hexagnoal **
    Return:
        d: the nearest distance at PBC 
        a2: atom 2 coordination after wrapping (atom 1 as center)
    
    Fortune Code:
        d = 0
        i = 0
        if (param(6) == 90) then
            ! orthogonal box
            a=param(1)
            b=param(2)
            x = x - FLOOR((x- x0 + a/2) / a) * a
            y = y - FLOOR((y- y0 + b/2) / b) * b
        end if
        if (param(6) == 120) then
            ! hexagonal box (90 90 120)
            a=param(1)
            i = FLOOR((y - y0 + a*0.866/2)/(a*0.866))
            y = y - i * (a*0.866)
            x = x + i * (a*0.5)
            x = x - FLOOR((x - x0 + a/2)/a) * a
        end if
        if (param(6) == 60) then
            ! hexagonal box (90 90 60)
            a=param(1)
            i = FLOOR((y - y0 + a*0.866/2)/(a*0.866))
            y = y - i * (a*0.866)
            x = x - i * (a*0.5)
            x = x - FLOOR((x - x0 + a/2)/a) * a
        end if
        d = sqrt((x-x0)**2+(y-y0)**2+(z-z0)**2)
    """
    if param[3] == 90. and param[4] == 90. and param[5] == 90.:
        a = param[0]
        b = param[1]
        a2[0] = a2[0] - np.floor((a2[0] - a1[0] + a / 2) / a) * a
        a2[1] = a2[1] - np.floor((a2[1] - a1[1] + b / 2) / b) * b
        d = np.linalg.norm(a1 - a2)
    elif param[3] == 90. and param[4] == 90. and param[5] == 120. and param[
            0] == param[1]:
        # hexagnoal (90 90 120)
        a = param[0]
        i = np.floor((a2[1] - a1[1] + a * 0.866 / 2) / (a * 0.866))
        a2[1] = a2[1] - i * (a * 0.866)
        a2[0] = a2[0] + i * (a * 0.5)
        a2[0] = a2[0] - np.floor((a2[0] - a1[0] + a / 2) / a) * a
        d = np.linalg.norm(a1 - a2)
    elif param[3] == 90. and param[4] == 90. and param[5] == 60. and param[
            0] == param[1]:
        # hexagnoal (90 90 60)
        a = param[0]
        i = np.floor((a2[1] - a1[1] + a * 0.866 / 2) / (a * 0.866))
        a2[1] = a2[1] - i * (a * 0.866)
        a2[0] = a2[0] - i * (a * 0.5)
        a2[0] = a2[0] - np.floor((a2[0] - a1[0] + a / 2) / a) * a
        d = np.linalg.norm(a1 - a2)
    return d, a2


@numba.njit
def get_2nd_dis(a1, a2, param):
    """
    get the 2nd mic distance at PBC

    Args:
        a1: atom 1 coordination [x, y, z]
        a2: **WARPPED** atom 2 coordination [x, y, z] 
        param: cell parameter [a, b, c, alpha, beta, gamma]
        ** only for cubic or hexagnoal **
    Return:
        d: the 2nd mic distance at PBC
    """
    d = np.zeros((3, 3))
    if param[3] == 90. and param[4] == 90. and param[5] == 90.:
        # orthogonal
        #if d_max > param[0] / 2. * 3. or d_max > param[1] / 2. * 3. :
        #warnings.warn('Cutoff Crosses the Cell!')
        v_a = np.zeros(3)
        v_b = np.zeros(3)
        v_a[0] = param[0]
        v_b[1] = param[1]
        #count = 0
        for i in range(-1, 2):
            for j in range(-1, 2):
                if i != 0 or j != 0:
                    a_new = a2 + i * v_a + j * v_b
                    #d[count] = np.linalg.norm(a1 - a_new)
                    #count = count + 1
                    d[i + 1][j + 1] = np.linalg.norm(a1 - a_new)
    elif param[3] == 90. and param[4] == 90. and param[0] == param[
            1] and param[5] == 120.:
        # hexagnoal (90 90 120)
        #if d_max > param[0] * np.sqrt(4) / 4. * 3. :
        #warnings.warn('Cutoff Crosses the Cell!')
        v_a = np.zeros(3)
        v_b = np.zeros(3)
        v_a[0] = param[0]
        v_b[0] = (-1) * param[0] / 2
        v_b[1] = param[0] * np.sqrt(3) / 2.
        #count = 0
        for i in range(-1, 2):
            for j in range(-1, 2):
                if i != 0 or j != 0:
                    a_new = a2 + i * v_a + j * v_b
                    #d[count] = np.linalg.norm(a1 - a_new)
                    #count = count + 1
                    d[i + 1][j + 1] = np.linalg.norm(a1 - a_new)
    elif param[3] == 90. and param[4] == 90. and param[5] == 60. and param[
            0] == param[1]:
        # hexagnoal (90 90 60)
        #if d_max > param[0] * np.sqrt(4) / 4. * 3. :
        #warnings.warn('Cutoff Crosses the Cell!')
        v_a = np.zeros(3)
        v_b = np.zeros(3)
        v_a[0] = param[0]
        v_b[0] = param[0] / 2
        v_b[1] = param[0] * np.sqrt(3) / 2.
        #count = 0
        for i in range(-1, 2):
            for j in range(-1, 2):
                if i != 0 or j != 0:
                    a_new = a2 + i * v_a + j * v_b
                    #d[count] = np.linalg.norm(a1 - a_new)
                    #count = count + 1
                    d[i + 1][j + 1] = np.linalg.norm(a1 - a_new)
    return d


@numba.njit
def get_rdf(count, bins):
    count = count.astype(np.float64)
    for idx, n in enumerate(count):
        v = 4. / 3. * np.pi * ((idx + 1)**3 - idx**3) * bins**3
        count[idx] = n / v
    return count


def get_one_c_atom_data(xyz_file,
         idx_c_atom,
         idxs_n_atom,
         param,
         d_min=0,
         d_max=6,
         nbins=100,
         n_layer=1):
    #start = time.process_time()
    count = np.zeros(nbins)
    bins = (d_max - d_min) / nbins
    for atoms in iread(xyz_file):
        diss = atoms.get_positions()
        a1 = diss[idx_c_atom]
        for n_idx in idxs_n_atom:
            a2 = diss[n_idx]
            d = get_dis(a1, a2, param, d_max, n_layer)
            if n_layer == 1:
                if d > 0 and d >= d_min and d < d_max:
                    count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1
            if n_layer == 2:
                _d = d.flatten()
                for d in _d:
                    if d > 0 and d >= d_min and d < d_max:
                        count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1
    rdf = get_rdf(count, bins)
    x = np.arange(len(rdf)) * bins + bins / 2
    rdf = np.concatenate(([x], [rdf]), axis=0)
    count = np.concatenate(([x], [count]), axis=0)
    #os.mkdir(output_dir)
    #np.save(output_dir + "count.npy", count)
    #np.save(output_dir + "rdf.npy", rdf)
    #fmt = "\n Work Completed! Used Time: {:.3f} seconds"
    #print(fmt.format(time.process_time() - start))
    return count, rdf


def get_multi_c_atom_data(xyz_file, idxs_c_atom, idxs_n_atom, param, d_min=0,
         d_max=6,
         nbins=100,
         n_layer=1):
    count = np.zeros(nbins)
    bins = (d_max - d_min) / nbins
    for atoms in iread(xyz_file):
        diss = atoms.get_positions()
        for c_idx in idxs_c_atom: 
            a1 = diss[c_idx]
            for n_idx in idxs_n_atom:
                a2 = diss[n_idx]
                d = get_dis(a1, a2, param, d_max, n_layer)
                if n_layer == 1:
                    if d > 0 and d >= d_min and d < d_max:
                        count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1
                if n_layer == 2:
                    _d = d.flatten()
                    for d in _d:
                        if d > 0 and d >= d_min and d < d_max:
                            count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1
    rdf = get_rdf(count, bins)
    x = np.arange(len(rdf)) * bins + bins / 2
    rdf = np.concatenate(([x], [rdf]), axis=0)
    count = np.concatenate(([x], [count]), axis=0)
    return count, rdf

def print_input(xyz_file, param, d_min, d_max, nbins, n_layer):
    print("**Input parameters**")
    print("input xyz trajectory: ", xyz_file)
    #print("index of central atom: ", idxs_c_atom)
    #print("symbol of neighbouring atom: ", symbol_n_atom)
    print("cell param: ", param)
    print("distance range in RDF: ", d_min, "A to", d_max, "A")
    print("num of bins: ", nbins)
    #print("output directory:", output_dir)
    if n_layer == 1:
        print("central cell only")
        print("====================")
        print("||    ||    ||    ||")
        print("||    ||    ||    ||")
        print("====================")
        print("||    ||xxxx||    ||")
        print("||    ||xxxx||    ||")
        print("====================")
        print("||    ||    ||    ||")
        print("||    ||    ||    ||")
        print("====================") 
    if n_layer == 2:
        print("9 cells in total")
        print("====================")
        print("||////||////||////||")
        print("||////||////||////||")
        print("====================")
        print("||////||xxxx||////||")
        print("||////||xxxx||////||")
        print("====================")
        print("||////||////||////||")
        print("||////||////||////||")
        print("====================") 
    return 0


"""
def main(xyz_file,
         idx_c_atom,
         symbol_n_atom,
         param,
         d_min=0,
         d_max=6,
         nbins=100,
         n_layer=1,
         output_dir="./rdf_data/"):
    start = time.process_time()

    count = np.zeros(nbins)
    bins = (d_max - d_min) / nbins
    atoms = read(xyz_file, index="0")
    idx_n_atom = get_idx_n_atom(atoms, idx_c_atom, symbol_n_atom)
    for atoms in iread(xyz_file):
        diss = atoms.get_positions()
        a1 = diss[idx_c_atom]
        for i in idx_n_atom:
            a2 = diss[i]
            d = get_dis(a1, a2, param, d_max, n_layer)
            if n_layer == 1:
                if d >= d_min and d < d_max:
                    count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1
            if n_layer == 2:
                _d = d.flatten()
                for d in _d:
                    if d >= d_min and d < d_max:
                        count[int(np.floor(d / bins))] = count[int(np.floor(d / bins))] + 1

    os.mkdir(output_dir)
    np.save(output_dir + "count.npy", count)
    rdf = get_rdf(count, bins)
    np.save(output_dir + "rdf.npy", rdf)
    fmt = "\n Work Completed! Used Time: {:.3f} seconds"
    print(fmt.format(time.process_time() - start))

"""