#!/bin/bash
sudo gfortran Domain.f90 Hydrodynamique.f90 Morphodynamique.f90 HydroMorphodynamique.f90 forcings.f90 main.f90 && ./a.out
