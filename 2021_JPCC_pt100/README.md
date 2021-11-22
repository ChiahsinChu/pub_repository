# README
test
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

#### rearrange trajectory

`gen_traj`: 

#### water density, orientational dipole and coverage

`wat_den`: codes used for data in Figure 2 (a) and (b), and Figure 4

- `wat_den/run.sh`: call `wat_den/watorient.f90` and generate required data 
- `wat_den/watorient.f90`: main code to calculate
- `wat_den/input.watori`: input parameters
- `wat_den/TRAJECTORY`: xyz trajectory for analysis

- `wat_den/o.dat`: 
- `wat_den/h.dat`: 
- `wat_den/ori.dat`: 
- `wat_den/inter.dat`: 


- water B orientational dipole in Figure S15

#### water configuration (psi and phi)

`wat_phi` and `wat_psi`: codes used for data in Figure 3 

- `*/run.sh`: call `wat_den/watorient.f90` and generate required data 
- `*/watorient.f90`: main code to calculate
- `*/input.watori`: input parameters
- `*/TRAJECTORY`: xyz trajectory for analysis

#### OH spatial distribution (heatmap)

codes used for data in Figure 5 and Figure S4

#### OH adsorption site

codes used for data in Figure 6

#### hydrogen bond analysis

codes used for data in Figure 7

#### Mulliken charge

codes used for data in Figure S10 and Figure S11

#### OH orientation distribution

Figure S13

#### OH spatial distribution

Figure S14

### Plots

`cd $dir/plot`

- `plot.ipynb`: jupyter notebook (version of the notebook server is 6.2.0 and is running on: Python 3.8.5)
- `data`: data for plotting

### AIMD trajectory

> Only Available Group-Internally

`cd $dir/md_traj`

- `*.bqb`: MD trajectory from AIMD simulations (compressed by [bqbtool]() for saving memory; only available group-internally)

### Manuscript

> Only Available Group-Internally

`cd $dir/manuscript`

- `main.tex`: last tex version of the manuscript
- `main.pdf`: last pdf version of the manuscript
- `si.tex`: last tex version of the supporting information
- `si.pdf`: last pdf version of the supporting information
- `achemso-demo.bib`: bibliography (bib) for `main.tex` and `si.tex`
- `*.pdf` other than `main.pdf` and `si.pdf`: figures in pdf



** Folder data_DFT-TestsSmallCoreCuPP:
Inputs, etc used for Table 1 and 2:
 
The pseudopotentials referenced in the pwscf.in files can be found in the pseudopotential directory, with the following equivalences:
Cu_myPPs_V01.UPF -> Cu.upf
H_myPPs_V01.UPF -> 01-H.GGA_JM.pspconvert.fhi_1s1.UPF
 
- for table 1 see info files in folders cohesiveEnergy and equationOfState
Code used to fit equation of state (only available group-internally): see python script FitEquationOfState.py in subfolder equationOfState/FitEquationOfState
- for table 2: data from dir_00059, dir_00060
 
 
**Folder data_coverageTests:
Input files for convergence tests for coverage (used in supplementary).
See info file in folder for more information on how information was extracted from the calculations.
The POTCAR file lies in the directory itself and was used in all subdirectories.
 
 
** Folder dmc_results:
These data where generated on cartesius (original folder name: Project_H2onCu111/runs_singePrecBlip_convThrs1E-16)
The analysis was done on my desktop (host 14)
 
The pseudopotentials referenced correspond to the the followingin pps in the pseudopotential directory:
Cu_jorg.casinoTab -> 29-Cu.GGA_MF_s17.pspconvert.fhi.casinoTab
Cu_myPPs_V01.casinoTab -> Cu.local2.casinoTab
H_jorg.casinoTab -> 01-H.GGA_JM.pspconvert.fhi.casinoTab
H_myPPs_V01.casinoTab -> 01-H.GGA_JM.pspconvert.fhi.casinoTab (i.e. same as H_jorg.casinoTab)
 
- jobDB.data: sqlite3 database containing the specifications of the run and the final results
- generatingPythonScripts: python scripts that can be used to automatically build the input from the database and get the results (only available group-internally)
- analysis_04.ipynb: jupyter notebook (version of the notebook server is 4.2.2 and is running on: Python 2.7.8)
- *.py files: contain methods needed by analysis_04.ipynb
 
