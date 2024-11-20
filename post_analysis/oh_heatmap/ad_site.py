
# coding: utf-8

from ase.io import iread, write
import numpy as np
import matplotlib.pyplot as plt
import math


nmetal = 96 #number of pt
nwat = 97 #number of water molecule
noh = 2 #number of OH per cell (two surfaces!)
cutoff = 0.2

nt = 0
nb = 0
data = []
for atoms in iread("./traj.xyz"):
    atoms.set_cell([11.246, 11.246, 35.94,90,90,90])
    atoms.set_pbc(pbc=True)
    #############  surface Pt  #############

    pt_min = 0
    pt_max = 0
    
    pt_pos=[]
    for pt in atoms:
        if pt.position[2]<atoms.cell[2][2]/2 and pt.symbol=='Pt':
            pt_pos.append(pt.position[2])
    pt_pos=np.array(pt_pos)
    pt_min=np.max(pt_pos)
    
    pt_pos=[]
    for pt in atoms:
        if pt.position[2]>atoms.cell[2][2]/2 and pt.symbol=='Pt':
            pt_pos.append(pt.position[2])
    pt_pos=np.array(pt_pos)
    pt_max=np.min(pt_pos)
    
    #############  surface Pt  #############

    for atom in atoms:
        if atom.index > 3 * nwat and atom.symbol=='O': # OH
            d0 = 4
            a = 0
            b = 0
            # find nearest Pt
            for pt in atoms:
                if pt.symbol=='Pt' and (pt.position[2]<pt_min+cutoff or pt.position[2]>pt_max-cutoff):
                    d=atoms.get_distance(atom.index,pt.index,mic=True)                    
                    if math.fabs(pt.position[2]-atom.position[2])>d:
                        d=d**2-(math.fabs(pt.position[2]-atom.position[2])-atoms.cell[2][2])**2
                    else:
                        d=d**2-(pt.position[2]-atom.position[2])**2
                    d=math.sqrt(d)
                    if d<d0:
                        d0=d
                        label=pt.index
            #print(d0,label)
            if atom.position[0]>atoms[label].position[0]:
                a=(atom.position[0]-atoms[label].position[0])-int((atom.position[0]\
               -atoms[label].position[0])/atoms.cell[0][0]+0.5)*atoms.cell[0][0]
            else:
                a=(atom.position[0]-atoms[label].position[0])-int((atom.position[0]\
               -atoms[label].position[0])/atoms.cell[0][0]-0.5)*atoms.cell[0][0]
            a=math.fabs(a)
            if atom.position[1]>atoms[label].position[1]:
                b=(atom.position[1]-atoms[label].position[1])-int((atom.position[1]\
               -atoms[label].position[1])/atoms.cell[1][1]+0.5)*atoms.cell[1][1]
            else:
                b=(atom.position[1]-atoms[label].position[1])-int((atom.position[1]\
               -atoms[label].position[1])/atoms.cell[1][1]-0.5)*atoms.cell[1][1]
            b=math.fabs(b)
            if a < 11.246 / 16:
                nt = nt + 1
            else:
                nb = nb + 1
            data.append([a,b])
            """
            f=open('pos.dat','a')
            f.write(str(a))
            f.write(r'  ')
            f.write(str(b))
            f.write(r'  ')
            f.write('\n')
            """         
#f.close()

np.savetxt("pos.dat", data)
print("ratio of top site OH:", nt / (nt + nb))
print("ratio of bridge site OH:", nb / (nt + nb))