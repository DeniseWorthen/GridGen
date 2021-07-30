subroutine write_cicegrid

   use param
   use grdvars
   use charstrings
   use icegriddefs
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: ii,id,rc, ncid, dim2(2)
  integer :: idimid,jdimid

  character(len=2)  :: vtype
  character(len=12) :: vname
  character(len=40) :: vlong
  character(len=12) :: vunit

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  call ice_typedefine

  fname_out= trim(dirout)//'grid_cice_NEMS_mx'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing CICE grid to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid, 'ni', ni, idimid)
  rc = nf90_def_dim(ncid, 'nj', nj, jdimid)

  do ii = 1,ncicevars
   vname = trim(icegrid(ii)%var_name)
   vlong = trim(icegrid(ii)%long_name)
   vunit = trim(icegrid(ii)%unit_name)
   vtype = trim(icegrid(ii)%var_type)

   dim2(:) =  (/idimid, jdimid/)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
   rc = nf90_put_att(ncid, id,     'units', vunit)
   rc = nf90_put_att(ncid, id, 'long_name', vlong)
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
