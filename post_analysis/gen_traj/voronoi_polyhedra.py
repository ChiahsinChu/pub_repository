import numpy as np
from ase import Atom, Atoms, io
import time


xyz_file = "traj.xyz"
cell_params = [11.246, 11.246, 35.94, 90, 90, 90]
output_file = "out.xyz"


def get_elem_idxs(atoms, elem_type):
    """
    get the list of index of neighbouring atoms

    Args:
        atoms: ASE Atoms object for analysis
        idx_c_atom: index of central atom for analysis
        symbol_n_atom: symbol of neighbouring atoms for analysis 
    Return:
        idx_n_atom: list of index of neighbouring atoms 
    """
    idxs = []
    for atom_idx, atom in enumerate(atoms):
        if atom.symbol == elem_type :
            idxs.append(atom_idx)
    return np.array(idxs)


def find_center(atoms, atom_idx, center_idx_list):
    ds = atoms.get_distances(atom_idx, center_idx_list, mic=True)
    center_idx = center_idx_list[np.argmin(ds)]
    return center_idx


def get_center_dict(center_idx_list):
    center_dict = {}
    for i in center_idx_list:
        center_dict[i] = []
    return center_dict


def main(xyz_file, cell_params, output_file):
    for atoms in io.iread(xyz_file):
        #print("start")
        atoms.set_cell(cell_params)
        atoms.set_pbc([True, True, False])
        h_idx_list = get_elem_idxs(atoms, "H")
        o_idx_list = get_elem_idxs(atoms, "O")
        pt_idx_list = get_elem_idxs(atoms, "Pt")
        o_dict = get_center_dict(o_idx_list)
        for h_idx in h_idx_list:
            o_idx = find_center(atoms, h_idx, o_idx_list) 
            #print(h_idx, o_idx)
            o_dict[o_idx].append(h_idx)

        new_atoms = Atoms()
        new_atom = Atom()
        # H2O in O-H-H
        for o_idx in o_idx_list:
            if len(o_dict[o_idx]) == 2:
                new_atom.symbol = "O"
                new_atom.position = atoms[o_idx].position
                new_atoms.extend(new_atom)
                new_atom.symbol = "H"
                new_atom.position = atoms[o_dict[o_idx][0]].position
                new_atoms.extend(new_atom)
                new_atom.position = atoms[o_dict[o_idx][1]].position
                new_atoms.extend(new_atom)  
        # Pt
        for pt_idx in pt_idx_list:
            new_atom.symbol = "Pt"
            new_atom.position = atoms[pt_idx].position
            new_atoms.extend(new_atom)
        # O-H
        for o_idx in o_idx_list:
            if len(o_dict[o_idx]) == 1:
                new_atom.symbol = "O"
                new_atom.position = atoms[o_idx].position
                new_atoms.extend(new_atom)
                new_atom.symbol = "H"
                new_atom.position = atoms[o_dict[o_idx][0]].position
                new_atoms.extend(new_atom)
        #print(new_atoms)
        #print("end")
        # write 
        if len(new_atoms) == len(atoms):
            io.write(output_file, new_atoms, append=True) 


if __name__ == '__main__':
    start = time.process_time()
    main(xyz_file, cell_params, output_file)
    fmt = "\n Work Completed! Used Time: {:.3f} seconds"
    print(fmt.format(time.process_time() - start))

