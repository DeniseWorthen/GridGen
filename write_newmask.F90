subroutine write_newmask
 
   use param
   use grdvars
   use charstrings
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: ii,id,rc, ncid, vardim(2)
  integer :: ni_dim,nj_dim

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  fname_out= trim(dirout)//'ocean_mask.ciceB.'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing new MOM6 mask to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid,'nx', ni, ni_dim)
  rc = nf90_def_dim(ncid,'nx', nj, nj_dim)

  vardim(2) = nj_dim
  vardim(1) = ni_dim
  rc = nf90_def_var(ncid, 'mask',  nf90_double, vardim, id)
  rc = nf90_put_att(ncid,     id,      'units', 'none')
  rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
  rc = nf90_enddef(ncid)

  rc = nf90_inq_varid(ncid,  'mask',      id)
  rc = nf90_put_var(ncid,        id,    real(wet4,8))

  rc = nf90_close(ncid)

end subroutine write_newmask
