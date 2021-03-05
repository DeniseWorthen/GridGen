subroutine write_newtopog(fname)
 
   use param
   use grdvars
   use charstrings
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname
  integer :: ii,id,rc, ncid, vardim(2)
  integer :: ni_dim,nj_dim

  real(kind=4), dimension(ni,nj) :: depth

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

  rc = nf90_open(fname, nf90_nowrite, ncid)
  print *, 'reading ocean depth from ',trim(fname)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  'depth',  id)
  rc = nf90_get_var(ncid,      id,  depth)
  rc = nf90_close(ncid)

  where(xwet .eq. 0.0)depth = 0.0

  rc = nf90_open(fname, nf90_write, ncid)

  rc = nf90_inq_varid(ncid,  'depth',  id)
  rc = nf90_put_var(ncid,      id,  depth)
  rc = nf90_close(ncid)

end subroutine write_newtopog
