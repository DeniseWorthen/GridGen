subroutine read_inputnml(fname)

 use grdvars,     only : nx,ny,ni,nj
 use grdvars,     only : editmask, debug
 use charstrings, only : dirsrc, dirout, res

  character(len=*),   intent(in) :: fname

  integer :: stderr, iounit, rc

  namelist /grid_nml/ ni, nj, dirsrc, dirout, res, editmask, debug

  ! Check whether file exists.
  inquire (file=trim(fname), iostat=rc)

  if (rc /= 0) then
      write (stderr, '(3a)') 'Error: input file "', trim(fname), '" does not exist.'
      return
  end if

  ! Open and read Namelist file.
  open (action='read', file=trim(fname), iostat=rc, newunit=iounit)
  read (nml=grid_nml, iostat=rc, unit=iounit)

  ! set supergrid dimensions
  nx = ni*2
  ny = nj*2

  if (rc /= 0) then
      write (stderr, '(a)') 'Error: invalid Namelist format.'
  end if

  close (iounit)
end subroutine read_inputnml
