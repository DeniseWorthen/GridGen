subroutine write_tripolegrid

   use param
   use grdvars
   use charstrings
   use fixgriddefs
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out
  integer :: ii,id,rc, ncid, dim2(2),dim3(3)
  integer :: ni_dim,nj_dim,nv_dim

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  ! define the output variables and file name
  call fixgrid_typedefine
  fname_out= trim(dirout)//'tripole.mx'//trim(res)//'.nc'
  print *,trim(fname_out)

  ! create the file
  ! 64_bit offset reqd for 008 grid
  ! produces b4b results for smaller grids
  rc = nf90_create(trim(fname_out), nf90_64bit_offset, ncid)
  print *, 'writing grid to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid,'ni', ni, ni_dim)
  rc = nf90_def_dim(ncid,'nj', nj, nj_dim)
  rc = nf90_def_dim(ncid,'nv', nv, nv_dim)
  
  !mask
  dim2(2) = nj_dim
  dim2(1) = ni_dim
   rc = nf90_def_var(ncid, 'wet', nf90_int, dim2, id)

  !area
  dim2(2) = nj_dim
  dim2(1) = ni_dim
   rc = nf90_def_var(ncid, 'area', nf90_double, dim2, id)
   rc = nf90_put_att(ncid, id,     'units',  'm2')

  !angleT
  dim2(2) = nj_dim
  dim2(1) = ni_dim
   rc = nf90_def_var(ncid, 'anglet', nf90_double, dim2, id)
   rc = nf90_put_att(ncid, id,     'units',  'radians')

  dim2(2) = nj_dim
  dim2(1) = ni_dim
  do ii = 1,ncoord
   rc = nf90_def_var(ncid, trim(fixgrid(ii)%var_name), nf90_double, dim2, id)
   rc = nf90_put_att(ncid, id,     'units', trim(fixgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(fixgrid(ii)%long_name))
   if(trim(fixgrid(ii)%var_name(1:3)) .eq. "lon")then
    rc = nf90_put_att(ncid, id,  'lon_bounds', trim(fixgrid(ii)%vertices))
   else
    rc = nf90_put_att(ncid, id,  'lat_bounds', trim(fixgrid(ii)%vertices))
   endif
  enddo
  dim3(3) = nv_dim
  dim3(2) = nj_dim
  dim3(1) = ni_dim
  do ii = ncoord+1,ncoord+nverts
   rc = nf90_def_var(ncid, trim(fixgrid(ii)%var_name), nf90_double, dim3, id)
   rc = nf90_put_att(ncid, id,     'units', trim(fixgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(fixgrid(ii)%long_name))
  enddo

  rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
  print *,trim(nf90_strerror(rc))
  rc = nf90_enddef(ncid)
  print *,trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,   'wet',        id)
  rc = nf90_put_var(ncid,        id, int(wet4))

  rc = nf90_inq_varid(ncid,  'area',      id)
  rc = nf90_put_var(ncid,        id,  areaCt)

  rc = nf90_inq_varid(ncid,'anglet',      id)
  rc = nf90_put_var(ncid,        id,  anglet)

  rc = nf90_inq_varid(ncid,  'lonCt',     id)
  rc = nf90_put_var(ncid,        id,   lonCt)

  rc = nf90_inq_varid(ncid,  'latCt',     id)
  rc = nf90_put_var(ncid,        id,   latCt)

  rc = nf90_inq_varid(ncid, 'lonCv',      id)
  rc = nf90_put_var(ncid,        id,   lonCv)

  rc = nf90_inq_varid(ncid, 'latCv',      id)
  rc = nf90_put_var(ncid,        id,   latCv)
  
  rc = nf90_inq_varid(ncid, 'lonCu',      id)
  rc = nf90_put_var(ncid,        id,   lonCu)

  rc = nf90_inq_varid(ncid, 'latCu',      id)
  rc = nf90_put_var(ncid,        id,   latCu)

  rc = nf90_inq_varid(ncid, 'lonBu',      id)
  rc = nf90_put_var(ncid,        id,   lonBu)

  rc = nf90_inq_varid(ncid, 'latBu',      id)
  rc = nf90_put_var(ncid,        id,   latBu)

  ! vertices
  rc = nf90_inq_varid(ncid,  'lonCt_vert',     id)
  rc = nf90_put_var(ncid,         id,  lonCt_vert)

  rc = nf90_inq_varid(ncid,  'latCt_vert',     id)
  rc = nf90_put_var(ncid,         id,  latCt_vert)

  rc = nf90_inq_varid(ncid, 'lonCv_vert',      id)
  rc = nf90_put_var(ncid,        id,   lonCv_vert)

  rc = nf90_inq_varid(ncid, 'latCv_vert',      id)
  rc = nf90_put_var(ncid,        id,   latCv_vert)

  rc = nf90_inq_varid(ncid, 'lonCu_vert',      id)
  rc = nf90_put_var(ncid,        id,   lonCu_vert)

  rc = nf90_inq_varid(ncid, 'latCu_vert',      id)
  rc = nf90_put_var(ncid,        id,   latCu_vert)

  rc = nf90_inq_varid(ncid, 'lonBu_vert',      id)
  rc = nf90_put_var(ncid,        id,   lonBu_vert)

  rc = nf90_inq_varid(ncid, 'latBu_vert',      id)
  rc = nf90_put_var(ncid,        id,   latBu_vert)

  rc = nf90_close(ncid)

end subroutine write_tripolegrid
