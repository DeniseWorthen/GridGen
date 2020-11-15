subroutine write_cicegrid
 
   use param
   use grdvars
   use charstrings
   use icegriddefs
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: ii,id,rc, ncid, vardim(2)
  integer :: ni_dim,nj_dim

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  call ice_typedefine

  fname_out= trim(dirout)//'grid_cice_NEMS_'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing CICE grid to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid,'ni', ni, ni_dim)
  rc = nf90_def_dim(ncid,'nj', nj, nj_dim)

  vardim(2) = nj_dim
  vardim(1) = ni_dim
  do ii = 1,ncicevars
   if(trim(icegrid(ii)%var_type) .eq. 'r8')rc = nf90_def_var(ncid, &
                  trim(icegrid(ii)%var_name), nf90_double, vardim, id)
   if(trim(icegrid(ii)%var_type) .eq. 'i4')rc = nf90_def_var(ncid, &
                  trim(icegrid(ii)%var_name), nf90_int,    vardim, id)
   rc = nf90_put_att(ncid, id,     'units', trim(icegrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(icegrid(ii)%long_name))
  enddo
   rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
   rc = nf90_enddef(ncid)

  rc = nf90_inq_varid(ncid,  'ulon',      id)
  rc = nf90_put_var(ncid,        id,    ulon)

  rc = nf90_inq_varid(ncid,  'ulat',      id)
  rc = nf90_put_var(ncid,        id,    ulat)

  rc = nf90_inq_varid(ncid,   'htn',      id)
  rc = nf90_put_var(ncid,        id,     htn)

  rc = nf90_inq_varid(ncid,   'hte',      id)
  rc = nf90_put_var(ncid,        id,     hte)
 
  rc = nf90_inq_varid(ncid,  'angle',     id)
  rc = nf90_put_var(ncid,         id,  angle)
 
  rc = nf90_inq_varid(ncid,    'kmt',        id)
  rc = nf90_put_var(ncid,         id, int(wet4))

  rc = nf90_close(ncid)

end subroutine write_cicegrid
