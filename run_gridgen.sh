#!/bin/bash
set -eux

function edit_namelist {

  sed -e "s/NI_GLB/$NI/g" \
      -e "s/NJ_GLB/$NJ/g" \
      -e "s|FIXDIR|$FIXDIR_PATH|g" \
      -e "s|OUTDIR|$OUTDIR_PATH|g" \
      -e "s|MOSAICDIR|$MOSAICDIR_PATH|g" \
      -e "s/TOPOGFILE/$TOPOGFILE/g" \
      -e "s/EDITSFILE/$EDITSFILE/g" \
      -e "s/RESNAME/$RESNAME/g" \
      -e "s/MOSAICRES/$MOSAICRES/g" \
      -e "s/NPX/$NPX/g" \
      -e "s/DO_MASKEDIT/$MASKEDIT/g" \
      -e "s/DO_DEBUG/$DEBUG/g" \
      -e "s/DO_POSTWGTS/$DO_POSTWGTS/g"
}

export RESNAME=$1
export MOSAICRES=$2
export DEBUG=.true.
export MASKEDIT=.false.
export DO_POSTWGTS=.false.
#export OUTDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/grids-20210727/
#export OUTDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/grids-esmf-20210822/
#export OUTDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/grids-esmf-20211107
export OUTDIR_PATH=/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210
export MOSAICDIR_PATH=/scratch1/NCEPDEV/global/glopara/fix/orog/20220805

if [ $RESNAME = 400 ]; then
  export FIXDIR_PATH=/scratch2/NCEPDEV/climate/Denise.Worthen/soca/test/Data/72x35x25/INPUT
else
  #export FIXDIR_PATH=/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp4/fix_mom6/${RESNAME}
  export FIXDIR_PATH=/scratch1/NCEPDEV/global/glopara/fix/mom6/20220805/${RESNAME}
fi

if [[ $MOSAICRES == C1152 ]]; then
    export NPX=1152
elif [[ $MOSAICRES == C384 ]]; then
    export NPX=384
elif [[ $MOSAICRES == C192 ]]; then
  export NPX=192
else
  export NPX=96
fi

if [ $RESNAME = 400 ]; then
  export NI=72
  export NJ=35
  export TOPOGFILE=ocean_topog.nc
  export EDITSFILE='none'
fi

if [ $RESNAME = 100 ]; then
  export NI=360
  export NJ=320
  export MASKEDIT=.T.
  export TOPOGFILE=topog.nc
  export EDITSFILE=topo_edits_011818.nc
  if [ $DO_POSTWGTS == .true. ]; then
   #pre-generate SCRIP files for dst rectilinear grids using NCO
   # TODO: is the stagger really correct? The first pt is at 0.0E?
   # should lat_type be cap? #lon_typ=grn_ctr#lat_typ=cap
   ncremap -g ${OUTDIR_PATH}/rect.1p0_SCRIP.nc -G latlon=181,360#lon_typ=grn_ctr
  fi
fi

if [ $RESNAME = 050 ]; then
  export NI=720
  export NJ=576
  export TOPOGFILE=ocean_topog.nc
  export EDITSFILE='none'
  if [ $DO_POSTWGTS == .true. ]; then
   #pre-generate SCRIP files for dst rectilinear grids using NCO
   # TODO: is the stagger really correct? The first pt is at 0.0E?
   # should lat_type be cap? #lon_typ=grn_ctr#lat_typ=cap
   ncremap -g ${OUTDIR_PATH}/rect.1p0_SCRIP.nc -G latlon=181,360#lon_typ=grn_ctr
   ncremap -g ${OUTDIR_PATH}/rect.0p5_SCRIP.nc -G latlon=361,720#lon_typ=grn_ctr
  fi
fi

if [ $RESNAME = 025 ]; then
  export NI=1440
  export NJ=1080
  export TOPOGFILE=ocean_topog.nc
  export EDITSFILE=All_edits.nc
  if [ $DO_POSTWGTS == .true. ]; then
   #pre-generate SCRIP files for dst rectilinear grids using NCO
   # TODO: is the stagger really correct? The first pt is at 0.0E?
   # should lat_type be cap? #lon_typ=grn_ctr#lat_typ=cap
   ncremap -g ${OUTDIR_PATH}/rect.1p0_SCRIP.nc -G latlon=181,360#lon_typ=grn_ctr
   ncremap -g ${OUTDIR_PATH}/rect.0p5_SCRIP.nc -G latlon=361,720#lon_typ=grn_ctr
   ncremap -g ${OUTDIR_PATH}/rect.0p25_SCRIP.nc -G latlon=721,1440#lon_typ=grn_ctr
  fi
fi

if [ ! -d ${OUTDIR_PATH} ]; then
  mkdir -p ${OUTDIR_PATH}
fi

edit_namelist < grid.nml.IN > grid.nml
make
#srun -A nems --ntasks=2 ./gengrid
srun -n 4 ./gengrid

# generate ice mesh
export FSRC=${OUTDIR_PATH}/Ct.mx${RESNAME}_SCRIP_land.nc
export FDST=${OUTDIR_PATH}/mesh.mx${RESNAME}.nc
srun -A nems -n 1 ESMF_Scrip2Unstruct ${FSRC} ${FDST} 0

# generate kmt file for CICE
export FSRC=${OUTDIR_PATH}/grid_cice_NEMS_mx${RESNAME}.nc
export FDST=${OUTDIR_PATH}/kmtu_cice_NEMS_mx${RESNAME}.nc
ncks -O -v kmt ${FSRC} ${FDST}

# clean up
#make clean
#rm grid.nml
