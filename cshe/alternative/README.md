# README

## calculation

- input files
  - `input.inp`
  - `coord.xyz`: single snapshots (as what you have in other CP2K calculations)
  - `reftraj.xyz`: a xyz trajectory files (what you get from AIMD, e.g., `*-pos-1.xyz`)
- output files
  - `output*`: CP2K output file (you can extract all Fermi level data from this)
  - `*hartree*.cube`: a series of hartree cube files

## analysis

```bash
  cd ./analysis/*
  # change the parameters in `run.sh` (and `aver.f90`)
  ./run.sh
```

