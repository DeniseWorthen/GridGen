module charstrings

  implicit none

  character(len=256) :: dirsrc, dirout
  character(len= 10) :: res

  character(len=100) :: maskfile = 'ocean_mask.nc'
  character(len= 12) :: maskname = 'mask'

  character(len=256) :: history
  character(len=  8) :: cdate

end module charstrings
