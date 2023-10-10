program test
  use ESMF

  implicit none
  character(len=120) :: fsrc, fdst, fwgt
  character(len=1024) :: cmdstr

  fsrc='/scratch1/NCEPDEV/climate/Denise.Worthen/grids-mesh-20231008/Ct.mx025_SCRIP.nc'
  fdst = '/scratch1/NCEPDEV/climate/Denise.Worthen/grids-mesh-20231008/Ct.mx025_mesh.nc'

  cmdstr = 'srun -A nems ESMF_Scrip2Unstruct '//trim(fsrc)//' '//trim(fdst)//' 0'
  print '(a)',trim(cmdstr)

  call execute_command_line(trim(cmdstr))

end program test
