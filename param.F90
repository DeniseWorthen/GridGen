module param

  implicit none

  ! number of vertices
  integer, parameter :: nv = 4

  ! ij offsets moving counter-clockwise around each Ct(i,j)
  integer, parameter, dimension(nv) :: iVertCt = (/0, -1, -1,  0/)
  integer, parameter, dimension(nv) :: jVertCt = (/0,  0, -1, -1/)

  integer, parameter :: ncoord = 2*4             ! 4sets of lat/lon pairs
  integer, parameter :: nverts = 2*4             ! 4sets of lat/lon pairs vertices
  integer, parameter ::  nvars = ncoord + nverts

  contains

   subroutine gengrid_config(ni,nj,nx,ny)

    integer, intent(out) :: ni,nj
    character(len=256) :: fname = 'grid_gen.nml'

    integer :: iunit, rc

    namelist /gridgen_nml/ ni, nj, dirsrc, dirout, res

      ! Check whether file exists.
      inquire (file=trim(fname), iostat=rc)

      if (rc /= 0) then
          write (stderr, '(3a)') 'Error: input file "', trim(fname), '" does not exist.'
          return
      end if

      ! Open and read Namelist file.
      open (action='read', file=trim(fname), iostat=rc, newunit=iounit)
      read (nml=gridgen_nml, iostat=rc, unit=iounit)

      nx = ni*2
      ny = nj*2

      if (rc /= 0) then
          write (stderr, '(a)') 'Error: invalid Namelist format.'
      end if

      close (iounit)
   end subroutine gengrid_config
end module param
