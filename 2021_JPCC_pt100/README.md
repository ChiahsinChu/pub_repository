# README

## DOI

10.1021/acs.jpcc.1c04895
 
## Title
 
Effects of Adsorbed OH on Pt(100)/Water Interfacial Structures and Potential
 
## Authors

Jia-Xin Zhu, Jia-Bo Le, Marc T.M. Koper, **Katharina Doblhoff-Dier**, **Jun Cheng**

## Contact e-mail
 
k.doblhoff-dier@lic.leidenuniv.nl

chengjun@xmu.edu.cn
 
## Abstract
 
Adsorbates at the electrode change the structure of the electrode/electrolyte interface. Despite the important influence of the interfacial structure on electrochemical processes, computational investigations targeting this influence are still lacking. Even the impact of one of the most common adsorbates, namely, adsorbed OH, is so far largely unknown. In this study, we choose the Pt(100)/water interface as a model system to investigate the interfacial water structure at various OH coverages with ab initio molecular dynamics. We find that the interfacial water structure is highly sensitive to the adsorption site of OH (namely, top or bridge site) and that the preference of adsorption sites of OH is, in turn, strongly influenced by the solvation caused by interfacial water. This indicates that the structure of water is correlated with that of OH. Based on a detailed analysis, we attribute these observations to a strong hydrogen-bonding network between OH and the interfacial water. This hydrogen-bonding network also results in a complicated dependence of the interfacial potential on the OH coverage, which is governed not only by the dipole induced by OH but also by the influence the OH species have on the interfacial water structure.
 
## Description per file

``` bash
cd 2021_JPCC_pt100
dir=$(pwd)
```
> **NOTE** 
> - There might be some alternative (and possibly simpler) methods for calculations and analysis, which are shown in `*/alternative` in the working directories `*/`.
> - Most of the codes here are written in fortran and *ad hoc*. Some of them are reconstructed in python modules for higher transferability, which can be found in [GitHub](https://github.com/ChiahsinChu/pub_repository/tree/master/2021_JPCC_pt100).
> - The parts with `Only Available Group-Internally` are available from the corresponding authors upon reasonable request.

### Shared files for QM calculation

`cd $dir/basis`

- `BASIS_MOLOPT`: basis set
- `GTH_POTENTIALS`: pseudopotentials
- `dftd3.dat`: Grimme D3 correction

### Preliminary tests

**Software: CP2K/4.1**

`cd $dir/test`

- `ener.inp`: cp2k input file for single point energy calculation
- `geo_opt.inp`: cp2k input file for geometry optimization

If calculations with unrestricted Kohn-Sham (`UKS`)/k-point grids (`KPOINTS`)/dipole correction (`SURFACE_DIPOLE_CORRECTION`) are required, turn on corresponding sections/keywords.

### Ab initio molecular dynamics (AIMD)

**Software: CP2K/4.1**

`cd $dir/aimd`

- `aimd.inp`: cp2k input file for AIMD

### Electrode potential v.s. SHE

**Software: CP2K/4.1**

`cd $dir/cshe`

- `extraction`: extract atomic positions from xyz trajectories
  - `extraction/run.sh`: call `extraction/extract.f90` and generate atomic positions for calculation
  - `extraction/extract.f90`: read and write atomic positions per 50 steps
  - `extraction/TRAJECTORY`: xyz trajectory file from AIMD for extracting atomic positions 
  - `extraction/coords.*`: output files (atomic positions of single snapshot)

- `inp1` & `inp2`: cp2k input file for single point energy calculation without atomic positions
- `script`: script to generate input file and save output files iteratively

- `analysis`
  - `analysis/fermi`
    - `analysis/fermi/run.sh`: bash file to extract Fermi levels from cp2k output files
  - `analysis/potential`
    - `analysis/potential/run.sh`: bash file to call `aver.f90` and `extract.f90`, and extract Fermi levels from cp2k hartree cube files
    - `analysis/potential/aver.f90`: average the cube file in xy direction
    - `analysis/potential/extract.f90`: average the hartree potential in the middle of the cell

### Interfacial potential difference

`cd $dir/int_pot`

- `extraction`: generate atomic positions based on the `coords.*` in `$dir/cshe/extraction`
- `top`: snapshots of interfaces in the top
- `bottom`: snapshots of interfaces in the bottom
- `top/cubecruncher` and `bottom/cubecruncher`: package from [CP2K](https://www.cp2k.org/tools:cubecruncher) for analysing cube file, run `run.sh` for extracting hartree potentials iteratively and calculating the potential drop (change two NR in line 14 of `run.sh` accordingly)

### Post analysis

`cd $dir/post_analysis`

#### generate trajectories for analysis

`gen_traj`: Since there is proton hopping happening in the Pt(100)/water interfaces with adsorbed OH, we cannot analyse water or OH structures directly. Instead, pre-processing about the raw AIMD trajectories is required.

- `gen_traj/voronoi_polyhedra.py`: codes used for distinguish water and OH from AIMD trajectories (find definition of Voronoi polyhedra in 10.1073/pnas.1819771116)
- `gen_traj/traj.xyz`: raw AIMD trajectories
- `gen_traj/out.xyz`: output trajectories in the order of OHH...Pt...OH... for further analysis

#### water density, orientational dipole and coverage

`wat_den`: codes used for data in Figure 2 (a) and (b), Figure 4 and Figure S15

- `wat_den/run.sh`: call `wat_den/watorient.f90` and generate required data 
- `wat_den/watorient.f90`: main code to calculate
- `wat_den/input.watori`: input parameters
- `wat_den/TRAJECTORY`: xyz trajectory for analysis
  
**output data**
- `wat_den/o.dat`: O density distribution (Figure 2 (a))
- `wat_den/h.dat`: H density distribution
- `wat_den/ori.dat`: \cos\psi distribution of water
- `wat_den/inter.dat`: \rho * \cos\psi (orientational dipole) distribution of water (Figure 2 (b))
- `wat_den/surf.out`: average positions of surfaces
- `wat_den/wat_cov.dat`: coverage of water A (2.7 A) and water B (4.5 A) in Figure 4
- `wat_den/watb_dip.dat`: water B orientational dipole in Figure S15

#### water configuration (psi and theta)

`wat_psi` and `wat_theta`: codes used for data in Figure 3, Figure S5 and Figure S9

- `*/run.sh`: call `*/watorient.f90` and generate `*/angle.dat`
- `*/watorient.f90`: code to calculate normalised distribution of angle (Here the code is for water A. If analysis of water B are required, change the IF criteria in line 209/225 (psi) and 209/235 (theta) accordingly.)
- `*/input.watori`: input parameters
- `*/TRAJECTORY`: xyz trajectory for analysis

- `*/angle.dat`: normalised distribution of angle


#### OH spatial distribution (heatmap) and top/bridge site OH ratio

`oh_heapmap`: codes used for data in Figure 5, Figure 6 and Figure S4 (print top site OH and bridge site OH ratio)

- `oh_heapmap/ad_site.py`: working code
- `oh_heapmap/traj.xyz`: input xyz trajectory
output
- `oh_heapmap/pos.dat`: x and y for heatmap 

#### hydrogen bond analysis

`h_bonds`: codes used for data in Figure 7

- `h-bonds/pre-process`: wrap & create a 3*3 cell (deal with the water at the boudary)
- `h-bonds/time_ave/*`: time average for acceptor or donor 
output
- `h-bonds/time_ave/*/hbond.out`: number of hydrogen bonds

#### Mulliken charge

`atomic_charge`: codes used to extract atomic charge from cp2k output (Figure S10 and Figure S11). 

- `atomic_charge/run.sh`: Copy file for single snapshot to this working directory iteratively and call `atomic_charge/watorient.f90` to extract 
- `atomic_charge/watorient.f90`: working codes
- `atomic_charge/input.watori`:input parameters
**output**
- `atomic_charge/bridge_oh.dat`, `atomic_charge/top_oh.dat`, `atomic_charge/wata.dat`, `atomic_charge/pt.dat`, `atomic_charge/total.dat`: average data ('mulliken O ','mulliken H ','mulliken total ','hirshfeld O ','hirshfeld H ','hirshfeld total')
- `atomic_charge/out-1.dat`, `atomic_charge/out-2.dat`, `atomic_charge/out-3.dat`, `atomic_charge/out-4.dat`, `atomic_charge/out-5.dat`: data at every snapshot for `atomic_charge/bridge_oh.dat`, `atomic_charge/top_oh.dat`, `atomic_charge/wata.dat`, `atomic_charge/pt.dat`, `atomic_charge/total.dat` 


#### OH spatial and orientation distribution

`oh_distribution`: codes used for data in Figure S13 and Figure S14

- `oh_distribution/run.sh`: call `oh_distribution/pos.f90` and `oh_distribution/ori.f90`
- `oh_distribution/pos.f90`: working code for OH position (Figure S13) 
- `oh_distribution/ori.f90`: working code for OH orientation (Figure S14)
**output**
- `oh_distribution/*pos.dat`: data in Figure S13
- `oh_distribution/*angle.dat`: data in Figure S14


### Plots

`cd $dir/plot`

- `plot.ipynb`: jupyter notebook (version of the notebook server is 6.2.0 and is running on: Python 3.8.5)
- `data`: data for plotting

### AIMD trajectory

> Only Available Group-Internally

`cd $dir/md_traj`

- `*.bqb`: MD trajectory from AIMD simulations (compressed by [bqbtool](https://brehm-research.de/bqb) for saving memory; only available group-internally)
- `*.xyz`: initial structures for AIMD simulations


### Manuscript

> Only Available Group-Internally

`cd $dir/manuscript`

- `main.tex`: last tex version of the manuscript
- `main.pdf`: last pdf version of the manuscript
- `si.tex`: last tex version of the supporting information
- `si.pdf`: last pdf version of the supporting information
- `achemso-demo.bib`: bibliography (bib) for `main.tex` and `si.tex`
- `*.pdf` other than `main.pdf` and `si.pdf`: figures in pdf

