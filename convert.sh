
#!/bin/bash

set -x

cmdpath="/scratch1/NCEPDEV/nems/role.epic/hpc-stack/libs/intel-2022.1.2/intel-2022.1.2/impi-2022.1.2/esmf/8.3.0b09/bin/"
fsrc="/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210/Ct.mx025_SCRIP_land.nc"
fdst="/scratch1/NCEPDEV/global/glopara/fix/orog/20220805/C3072/C3072_mosaic.nc"
fwgt="/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210/Ct.mx025.to.C3072.nc"
fatm="/scratch1/NCEPDEV/global/glopara/fix/orog/20220805/C3072/"
#fdst="/scratch1/NCEPDEV/global/glopara/fix/orog/20220805/C1152/C1152_mosaic.nc"
#fwgt="/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210/Ct.mx025.to.C1152.nc"
#fatm="/scratch1/NCEPDEV/global/glopara/fix/orog/20220805/C1152/"

#mpirun -np 4 ESMF_RegridWeightGen -s src.nc -d dst.nc -m conserve -w w.nc

#time srun -n 20 $cmdpath/ESMF_RegridWeightGen -s $fsrc -d $fdst -m conserve -w $fwgt --ignore_unmapped --ignore_degenerate --64bit_offset --tilefile_path $fatm

time srun -n 38 $cmdpath/ESMF_RegridWeightGen -s $fsrc -d $fdst -m conserve -w $fwgt --ignore_unmapped --ignore_degenerate --64bit_offset --tilefile_path $fatm

#sbatch -A nems --ntasks-per-node=10 --nodes=1 -t 30 -q debug --wrap "time $cmdpath/ESMF_RegridWeightGen -s $fsrc -d $fdst -m conserve -w $fwgt --ignore_unmapped --ignore_degenerate --64bit_offset --tilefile_path $fatm"
