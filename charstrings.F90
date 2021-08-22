module charstrings

  implicit none

  integer, parameter :: CL = 256
  integer, parameter :: CM =  64
  integer, parameter :: CS =  24

  character(len=CL) :: dirsrc, dirout, fv3dir
  character(len=CS) :: res, atmres

  character(len=CL) :: maskfile = 'ocean_mask.nc'
  character(len=CS) :: maskname = 'mask'

  character(len=CL) :: history
  character(len=CS) :: cdate

end module charstrings
