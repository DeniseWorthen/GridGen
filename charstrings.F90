module charstrings

  implicit none

  integer, parameter :: CL = 256
  integer, parameter :: CS =  12

  character(len=CL) :: dirsrc, dirout
  character(len=CS) :: res

  character(len=CL) :: maskfile = 'ocean_mask.nc'
  character(len=CS) :: maskname = 'mask'

  character(len=CL) :: history
  character(len=CS) :: cdate

end module charstrings
