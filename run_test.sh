#!/bin/bash
set -eux

function edit_namelist {

  sed -e "s/NI_GLB/$NI/g" \
      -e "s/NJ_GLB/$NJ/g" \
      -e "s|FIXDIR|$FIXDIR_PATH|g" \
      -e "s|OUTDIR|$OUTDIR_PATH|g" \
      -e "s/RESNAME/$RESNAME/g" \
      -e "s/DO_MASKEDIT/$MASKEDIT/g" \
      -e "s/DO_DEBUG/$DEBUG/g"
}

export RESNAME=$1
export DEBUG=.F.
export MASKEDIT=.F.
#export OUTDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/grids-20210727/
export OUTDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/grids-ufs-20210822/

if [ $RESNAME = 400 ]; then
  export FIXDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/soca/test/Data/72x35x25/INPUT/
else
  export FIXDIR_PATH=/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp4/fix_mom6/${RESNAME}/
fi

if [ $RESNAME = 400 ]; then
  export NI=72
  export NJ=35
fi

if [ $RESNAME = 100 ]; then
  export NI=360
  export NJ=320
  export MASKEDIT=.T.
fi

if [ $RESNAME = 050 ]; then
  export NI=720
  export NJ=576
fi

if [ $RESNAME = 025 ]; then
  export NI=1440
  export NJ=1080
fi

edit_namelist < grid.nml.IN > grid.nml
make
./gengrid

# clean up
make clean
rm grid.nml