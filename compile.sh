#!/bin/bash

PREREQ_DIR=${PREREQ_DIR:-${HOME}/opt-gcc}

export PATH=${PREREQ_DIR}/bin:$PATH
export LD_LIBRARY_PATH=${PREREQ_DIR}/lib:$LD_LIBRARY_PATH

gfortran -O3 -o extract_prec mod_out.f90 mod_netcdf.f90  mod_polygon_point_chek.f90 \
   extract_prec.f90 -I${PREREQ_DIR}/include -L${PREREQ_DIR}/lib -lnetcdff -lnetcdf -ffree-form -ffree-line-length-none
