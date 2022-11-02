#!/bin/bash
sudo gfortran Domain.f90 HydroMorphodynamique.f90 forcings.f90 main.f90 && ./a.out
sudo rm *.bak
sudo rm *.mod
