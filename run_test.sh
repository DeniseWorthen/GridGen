#!/bin/bash
set -eux

function edit_namelist {

  sed -e "s/NI_GLB/$NI/g" \
      -e "s/NJ_GLB/$NJ/g" \
      -e "s|FIXDIR|$FIXDIR_PATH|g" \
      -e "s|OUTDIR|$OUTDIR_PATH|g" \
      -e "s/RESNAME/$GRDNAME/g" \
      -e "s/DO_MASKEDIT/$MASKEDIT/g" \
      -e "s/DO_DEBUG/$DEBUG/g"
}

export NI='360'
export NJ='320'
export GRDNAME='100'
export MASKEDIT='.T.'
export DEBUG='.F.'
export FIXDIR_PATH='/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp4/fix_mom6/'
export OUTDIR_PATH='/scratch2/NCEPDEV/climate/Denise.Worthen/grids-ufs/'

edit_namelist < grid.nml.IN > grid.nml
make
./gengrid

export NI='1440'
export NJ='1080'
export GRDNAME='025'
export MASKEDIT='.F.'

edit_namelist < grid.nml.IN > grid.nml
./gengrid

make clean
