module merra2

  use gengrid_kinds, only : dbl_kind, real_kind, int_kind
  use vartypedefs,   only: maxvars, merra2vars, merra2vars_typedefine
  use charstrings,   only: merra2dir
  use netcdf

  implicit none

  ! hard-coded for merra2 files
  integer, parameter :: nlons = 576, nlats = 361, nlevs = 72, nmons = 12, ntra = 15

  subroutine merra2_vars

   ! local variables
   character(len=CM) :: fname = 'merra2.aerclim.2003-2014.m01.nc'

   integer :: ncid,rc,nvars
 
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

  rc = nf90_open(trim(merra2dir)//trim(fname), nf_nowrite, ncid)
  rc = nf90_inquire(ncid, nVariables=nvars)
  rc = nf90_close(ncid)

  print *,nvars
  end subroutine merra2_vars
end module merra2
