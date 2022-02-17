module scripgrid

  use gengrid_kinds, only: dbl_kind,int_kind,CM
  use grdvars,       only: nv,mastertask
  use charstrings,   only: logmsg
  use vartypedefs,   only: maxvars, scripvars, scripvars_typedefine
  use netcdf

  implicit none
  private

  public write_scripgrid

  contains

  subroutine write_scripgrid(fname,lx,ly,lats,lons,vlats,vlons,imask)

   character(len=*) , intent(in) :: fname
   integer(int_kind), intent(in) :: lx,ly
   real(dbl_kind)   , intent(in) :: lons(lx,ly),lats(lx,ly)
   real(dbl_kind)   , intent(in) :: vlats(lx,ly,nv),vlons(lx,ly,nv)
   integer(int_kind), intent(in), optional :: imask(lx,ly)

   ! local variables
   integer, parameter :: grid_rank = 2

   integer :: ii,n,id,rc, ncid, dim2(2),dim1(1)
   integer :: idimid,jdimid,kdimid

   integer, dimension(grid_rank) :: gdims
   integer(int_kind), dimension(lx*ly)    :: cnmask          !1-d mask
   real(dbl_kind),    dimension(lx*ly)    :: cnlons, cnlats  !1-d center lats,lons
   real(dbl_kind),    dimension(nv,lx*ly) :: crlons, crlats  !2-d corner lats,lons

   real(dbl_kind), dimension(lx,ly) :: tmp

   character(len=2)  :: vtype
   character(len=CM) :: vname
   character(len=CM) :: vunit

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
 
  gdims(:) = (/lx,ly/)
  cnlons = reshape(lons, (/lx*ly/))
  cnlats = reshape(lats, (/lx*ly/))
  do n = 1,nv
      tmp(:,:) = vlons(:,:,n)
   crlons(n,:) = reshape(tmp, (/lx*ly/))
      tmp(:,:) = vlats(:,:,n)
   crlats(n,:) = reshape(tmp, (/lx*ly/))
  end do

  if(present(imask))then
   cnmask = reshape(imask, (/lx*ly/))
  else
   cnmask = 1
  end if

!---------------------------------------------------------------------
! create the netcdf file
!---------------------------------------------------------------------

  ! define the output variables and file name
  call scripvars_typedefine
  ! create the file
  ! 64_bit offset reqd for 008 grid
  ! produces b4b results for smaller grids
  rc = nf90_create(trim(fname), nf90_64bit_offset, ncid)
  if(mastertask) then
    logmsg = '==> writing SCRIP grid to '//trim(fname)
    print '(a)',trim(logmsg)
    if(rc .ne. 0)print '(a)', 'nf90_create = '//trim(nf90_strerror(rc))
  end if

  rc = nf90_def_dim(ncid, 'grid_size',     lx*ly, idimid)
  rc = nf90_def_dim(ncid, 'grid_corners',     nv, jdimid)
  rc = nf90_def_dim(ncid, 'grid_rank', grid_rank, kdimid)
  
  !grid_dims
  dim1(:) = (/kdimid/)
  rc = nf90_def_var(ncid, 'grid_dims', nf90_int, dim1, id)
  ! mask
  dim1(:) = (/idimid/)
  rc = nf90_def_var(ncid, 'grid_imask', nf90_int, dim1, id)
  rc = nf90_put_att(ncid, id,     'units',      'unitless')

  ! centers
  do ii = 1,2
   vname = trim(scripvars(ii)%var_name)
   vunit = trim(scripvars(ii)%unit_name)
   vtype = trim(scripvars(ii)%var_type)
   dim1(:) =  (/idimid/)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim1, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim1, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim1, id)
   rc = nf90_put_att(ncid, id,     'units', vunit)
  enddo

  ! corners
  do ii = 3,4
   vname = trim(scripvars(ii)%var_name)
   vunit = trim(scripvars(ii)%unit_name)
   vtype = trim(scripvars(ii)%var_type)
   dim2(:) =  (/jdimid,idimid/)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
   rc = nf90_put_att(ncid, id,     'units', vunit)
  enddo
  rc = nf90_enddef(ncid)

  rc = nf90_inq_varid(ncid,  'grid_dims',        id)
  rc = nf90_put_var(ncid,             id,     gdims)
  rc = nf90_inq_varid(ncid, 'grid_imask',        id)
  rc = nf90_put_var(ncid,             id,    cnmask)

  rc = nf90_inq_varid(ncid,  'grid_center_lon',        id)
  rc = nf90_put_var(ncid,                   id,    cnlons)
  rc = nf90_inq_varid(ncid,  'grid_center_lat',        id)
  rc = nf90_put_var(ncid,                   id,    cnlats)

  rc = nf90_inq_varid(ncid,  'grid_corner_lon',        id)
  rc = nf90_put_var(ncid,                   id,    crlons)
  rc = nf90_inq_varid(ncid,  'grid_corner_lat',        id)
  rc = nf90_put_var(ncid,                   id,    crlats)

  rc = nf90_close(ncid)

  end subroutine write_scripgrid
end module scripgrid
