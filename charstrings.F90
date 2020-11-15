module charstrings

  implicit none

#ifdef output_grid_qdeg
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/MOM6_FIX/025/'
  character(len= 10) :: res = 'mx025'
#endif
#ifdef output_grid_hdeg
! temporary location
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/MOM6_FIX/050/'
  character(len= 10) :: res = 'mx050'
#endif
#ifdef output_grid_1deg
! temporary location
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/MOM6_FIX/100/'
  character(len= 10) :: res = 'mx100'
#endif
  character(len=100) :: maskfile = 'ocean_mask.nc'
  character(len= 12) :: maskname = 'mask'

  !character(len=256) :: dirout = '/scratch2/NCEPDEV/climate/Denise.Worthen/TTout2/'
  character(len=256) :: dirout = '/scratch2/NCEPDEV/climate/Denise.Worthen/Test/'
  character(len=256) :: history
  character(len=  8) :: cdate

end module charstrings
