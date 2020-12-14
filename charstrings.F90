module charstrings

  implicit none

  !character(len=256) :: dirsrc = &
  ! '/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_mom6/'
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/Huiskamp/INPUT/'
#ifdef output_grid_qdeg
  character(len= 10) :: res = '025'
#endif
#ifdef output_grid_hdeg
  character(len= 10) :: res = '050'
#endif
#ifdef output_grid_1deg
  character(len= 10) :: res = '100'
#endif
#ifdef output_grid_3deg
  character(len= 10) :: res = '300'
#endif
  character(len=100) :: maskfile = 'ocean_mask.nc'
  character(len= 12) :: maskname = 'mask'

  character(len=256) :: dirout = '/scratch2/NCEPDEV/climate/Denise.Worthen/GRIDS/'
  character(len=256) :: history
  character(len=  8) :: cdate

end module charstrings
