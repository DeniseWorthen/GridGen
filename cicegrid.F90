module cicegrid

   use grdvars
   use charstrings
   use vartypedefs, only: maxvars, cicevars, cicevars_typedefine
   use netcdf

   implicit none

  contains

  subroutine write_cicegrid

  ! local variables

  character(len=CL) :: fname_out
  integer :: ii,id,rc, ncid, dim2(2)
  integer :: idimid,jdimid

  character(len=2)  :: vtype
  character(len=CM) :: vname
  character(len=CM) :: vlong
  character(len=CM) :: vunit

!---------------------------------------------------------------------
! create the netcdf file
!---------------------------------------------------------------------

  ! define the output variables and file name
  call cicevars_typedefine
  fname_out= trim(dirout)//'grid_cice_NEMS_mx'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print '(a)', 'writing CICE grid to '//trim(fname_out)
  if(rc .ne. 0)print '(a)', 'nf90_create = '//trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid, 'ni', ni, idimid)
  rc = nf90_def_dim(ncid, 'nj', nj, jdimid)

  do ii = 1,maxvars
   if(len_trim(cicevars(ii)%var_name) .gt. 0)then
     vname = trim(cicevars(ii)%var_name)
     vlong = trim(cicevars(ii)%long_name)
     vunit = trim(cicevars(ii)%unit_name)
     vtype = trim(cicevars(ii)%var_type)

     dim2(:) =  (/idimid, jdimid/)
     if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
     if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
     if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
     rc = nf90_put_att(ncid, id,     'units', vunit)
     rc = nf90_put_att(ncid, id, 'long_name', vlong)
   end if
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
end module cicegrid
