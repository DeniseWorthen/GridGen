subroutine add_topoedits

   use param
   use grdvars
   use charstrings
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: dimid,i,ii,id,rc, ncid, dim1(1), dim2(2), ni_dim, nj_dim
  integer :: nvals, newvals

  integer, allocatable, dimension(:) :: orii, orij, newi, newj
  real(kind=4), allocatable, dimension(:) :: oriz, newz

   fname_in =  trim(dirsrc)//trim(res)//'/'//'topo_edits_011818.nc'
  fname_out = trim(dirout)//'ufs.topo_edits_011818.nc'

!---------------------------------------------------------------------
! read existing topo edits
!---------------------------------------------------------------------

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *,'using topo edits file ',trim(fname_in)

  rc = nf90_inq_dimid(ncid, 'nEdits', dimid)
  rc = nf90_inquire_dimension(ncid, dimid, len=nvals)
  rc = nf90_close(ncid)

  ! return the existing values
  allocate(orii(nvals))
  allocate(orij(nvals))
  allocate(oriz(nvals))

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  rc = nf90_inq_varid(ncid, 'iEdit', id)
  rc = nf90_get_var(ncid, id, orii)
  rc = nf90_inq_varid(ncid, 'jEdit', id)
  rc = nf90_get_var(ncid, id, orij)
  rc = nf90_inq_varid(ncid, 'zEdit', id)
  rc = nf90_get_var(ncid, id, oriz)
  rc = nf90_close(ncid)

!---------------------------------------------------------------------
! determine the number of points to be added
! For now, only close j=1
!---------------------------------------------------------------------

  ii = 0
  do i = 1,ni
   if(wet4(i,1) .eq. 1.0)ii = ii+1
  end do
  newvals = nvals+ii
  print *, 'found ',ii,' open water points at j=1 , newvals = ',newvals
 
  allocate(newi(1:newvals))
  allocate(newj(1:newvals))
  allocate(newz(1:newvals))

  newi(1:nvals) = orii(1:nvals)
  newj(1:nvals) = orij(1:nvals)
  newz(1:nvals) = oriz(1:nvals)

  ii = nvals
  do i = 1,ni
   if(wet4(i,1) .eq. 1.0)then
     ii = ii+1
     newi(ii) = i-1
     newj(ii) = 1-1
     newz(ii) = 0.0
   end if
  end do

  !do i = 1,newvals
  ! print *,i,newi(i),newj(i),newz(i)
  !end do

!---------------------------------------------------------------------
! create new topoedits file
!---------------------------------------------------------------------

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing new topo edits to ',trim(fname_out)

  rc = nf90_def_dim(ncid, 'nEdits', newvals, ni_dim)

  rc = nf90_def_var(ncid,    'ni', nf90_int,   id)
  rc = nf90_def_var(ncid,    'nj', nf90_int,   id)

  dim1(1) = ni_dim
  rc = nf90_def_var(ncid, 'iEdit', nf90_int,   dim1, id)
  rc = nf90_def_var(ncid, 'jEdit', nf90_int,   dim1, id)
  rc = nf90_def_var(ncid, 'zEdit', nf90_float, dim1, id)
  rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
  rc = nf90_enddef(ncid)
 
  rc = nf90_inq_varid(ncid,     'ni',     id)
  print *,trim(nf90_strerror(rc))
  rc = nf90_put_var(ncid,         id,     ni)
  print *,trim(nf90_strerror(rc))
  rc = nf90_inq_varid(ncid,     'nj',     id)
  rc = nf90_put_var(ncid,         id,     nj)

  rc = nf90_inq_varid(ncid,  'iEdit',     id)
  rc = nf90_put_var(ncid,         id,    newi)
  rc = nf90_inq_varid(ncid,  'jEdit',     id)
  rc = nf90_put_var(ncid,         id,    newj)
  rc = nf90_inq_varid(ncid,  'zEdit',     id)
  rc = nf90_put_var(ncid,         id,    newz)

  rc = nf90_close(ncid)

end subroutine add_topoedits
