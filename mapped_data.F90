module mapped_data

  use gengrid_kinds, only : dbl_kind,real_kind,int_kind,CL,CM,CS
  use grdvars,       only : ni,nj,npx,mastertask
  use charstrings,   only : dirout,merra2dir,atmres,logmsg
  use vartypedefs,   only: maxvars, merra2vars, merra2vars_typedefine
  use netcdf

  implicit none

  integer, parameter :: ntile = 6

  ! hard-coded for merra2 files
  integer, parameter :: nlons = 576, nlats = 361, nlev = 72, nmon = 12, ntra = 15+1
  real(real_kind)    :: vmiss = 1.0e-15

  ! local variables
  character(len= 2)  :: cmon
  character(len=CS)  :: ctile
  character(len=CL)  :: fsrc,fdst
  character(len=CM)  :: fname = 'merra2.aerclim.2003-2014.m'
  character(len=CS)  :: specname(ntra) = (/'DU001   ','DU002   ','DU003   ','DU004   ', &
                                           'DU005   ', &
                                           'SS001   ','SS002   ','SS003   ','SS004   ', &
                                           'SS005   ','SO4     ', &
                                           'BCPHOBIC','BCPHILIC','OCPHOBIC','OCPHILIC', &
                                           'DELP    '/)
  character(len=CM) :: vname
  character(len=2)  :: vtype
  character(len=CM) :: vlong
  character(len=CM) :: vunit
  character(len=8)  :: i2fmt = '(i2.2)'

  integer :: ncid,id,rc,xtype,i,nt,nm
  integer :: idimid, jdimid,kdimid,timid
  integer :: dim1(1),dim2(2),dim3(3),dim4(4)

  contains

  subroutine setup_merra2files

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

    cmon = '01'
    fsrc=trim(merra2dir)//'/'//trim(fname)//trim(cmon)//'.nc'
    rc = nf90_open(trim(fsrc), nf90_nowrite, ncid)
    do nt = 1,ntra
     vname = trim(specname(nt))

     rc = nf90_inq_varid(ncid, trim(vname), id)
     rc = nf90_inquire_variable(ncid, id, xtype=xtype)
     rc = nf90_get_att(ncid, id, 'long_name',   vlong)
     rc = nf90_get_att(ncid, id, 'units',       vunit)

     merra2vars(nt)%var_name   = trim(vname)
     merra2vars(nt)%long_name  = trim(vlong)
     merra2vars(nt)%unit_name  = trim(vunit)
     if(xtype .eq. 5)merra2vars(nt)%var_type   = 'r4'
     if(xtype .eq. 6)merra2vars(nt)%var_type   = 'r8'
    enddo
     rc = nf90_close(ncid)

    !do nt = 1,ntra
    ! print '(5a)',trim(merra2vars(nt)%var_name),  '  ', &
    !              trim(merra2vars(nt)%long_name), '  ', &
    !              trim(merra2vars(nt)%unit_name)
    !end do

    !define netcdf tile file monthly output
    do nm=1,nmon
     write( cmon, i2fmt)nm
     do i = 1,ntile
      write(ctile,'(a5,i1)')'.tile',i
      fdst=trim(dirout)//'/'//trim(atmres)//'.'//trim(fname)//trim(cmon)//trim(ctile)//'.nc'
      if(mastertask) then
        logmsg = 'creating mapped MERRA2 file '//trim(fdst)
        print '(a)',trim(logmsg)
      end if

       rc = nf90_create(trim(fdst), nf90_64bit_offset, ncid)
       rc = nf90_def_dim(ncid, 'grid_xt', npx, idimid)
       rc = nf90_def_dim(ncid, 'grid_yt', npx, jdimid)
       rc = nf90_def_dim(ncid, 'lev',    nlev, kdimid)
       rc = nf90_def_dim(ncid, 'time', nf90_unlimited,  timid)

       dim2(:) =  (/idimid, jdimid/)
       vname = 'grid_xt'
       rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
       vname = 'grid_yt'
       rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
       dim1(:) =  (/kdimid/)
       
       dim4(:) =  (/idimid, jdimid,kdimid,timid/)
       do nt = 1,ntra
         vname = trim(merra2vars(nt)%var_name)
         vlong = trim(merra2vars(nt)%long_name)
         vunit = trim(merra2vars(nt)%unit_name)
         vtype = trim(merra2vars(nt)%var_type)
        if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim4, id)
        if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim4, id)
        if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim4, id)
        rc = nf90_put_att(ncid, id,      'units', vunit)
        rc = nf90_put_att(ncid, id,  'long_name', vlong)
        rc = nf90_put_att(ncid, id, '_FillValue', vmiss)
       end do; ntra
        rc = nf90_enddef(ncid)
        rc = nf90_close(ncid)
     end do; ntile
    enddo; nmon

  end subroutine setup_merra2files

  subroutine make_tiled_data(src, wgt)
  character(len=*), intent(in) :: src, wgt

  ! local variables
  integer(int_kind) :: n_a, n_b, n_s

  integer(int_kind), allocatable, dimension(:) :: col, row
     real(dbl_kind), allocatable, dimension(:) :: S
     real(dbl_kind), allocatable, dimension(:) :: lat1d, lon1d

     real(real_kind), allocatable, dimension(:,:) :: src_field
     real(real_kind), allocatable, dimension(:,:) :: dst_field

     real(real_kind), allocatable, dimension(:,:,:) :: dst3d
     real(dbl_kind),  allocatable, dimension(:,:,:) :: lat3d,lon3d

     real(real_kind), allocatable, dimension(:,:)   :: dst2d
     real(dbl_kind), allocatable, dimension(:,:)   :: lat2d,lon2d

  ! MERRA2 level data
  real(real_kind) :: mdat(nlons,nlats,nlev)

  integer :: ii,jj
  integer :: istr,iend

!---------------------------------------------------------------------
! retrieve the merra2 variable definitions
!---------------------------------------------------------------------

  call setup_merra2files

#ifdef test
!---------------------------------------------------------------------
! retrieve the weights
!---------------------------------------------------------------------

    rc = nf90_open(trim(wgt), nf90_nowrite, ncid)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)
  
    allocate(col(1:n_s))
    allocate(row(1:n_s))
    allocate(  S(1:n_s))

    allocate(lat1d(1:n_b))
    allocate(lon1d(1:n_b))

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)

    ! 1d-tiled lat,lon
    rc = nf90_inq_varid(ncid, 'yc_b',     id)
    rc = nf90_get_var(ncid,       id,  lat1d)
    rc = nf90_inq_varid(ncid, 'xc_b',     id)
    rc = nf90_get_var(ncid,       id,  lon1d)
    rc = nf90_close(ncid)

    allocate(src_field(1:n_a,1:nlev))
    allocate(dst_field(1:n_b,1:nlev))

!---------------------------------------------------------------------
! retrieve MERRA2 data from the file and map it
!---------------------------------------------------------------------

    do nm=1,nmon
     write( cmon, i2fmt)nm

     fsrc=trim(merra2dir)//'/'//trim(fname)//trim(cmon)//'.nc'
     rc = nf90_open(trim(fsrc), nf90_nowrite, ncid)
     do nt = 1,ntra
      vname = trim(specname(nt))
      rc = nf90_inq_varid(ncid, vname,   id)
      rc = nf90_get_var(ncid,      id, mdat)
      rc = nf90_close(ncid)
    
        ii = 0
      do j = 1,nlats
       do i = 1,nlons
         ii = ii + 1
         src_field(ii,:) = mdat(i,j,:)
       enddo
      enddo

    do i = 1,n_s
      ii = row(i); jj = col(i)
      dst_field(ii) = dst_field(ii) + S(i)*real(src_field(jj),dbl_kind)
    enddo

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

    allocate(dst3d(npx,npx,ntile))
    allocate(lon3d(npx,npx,ntile)); allocate(lat3d(npx,npx,ntile))
    allocate(dst2d(npx,npx))
    allocate(lon2d(npx,npx)); allocate(lat2d(npx,npx))

    do i = 0,ntile-1
     istr = i*npx*npx+1
     iend = istr+npx*npx-1
     !print *,i,istr,iend
     dst3d(:,:,i+1) = reshape(dst_field(istr:iend), (/npx,npx/))
     lat3d(:,:,i+1) = reshape(    lat1d(istr:iend), (/npx,npx/))
     lon3d(:,:,i+1) = reshape(    lon1d(istr:iend), (/npx,npx/))
    end do

    do i = 1,ntile
     write(ctile,'(a5,i1)')'.tile',i
     fdst = trim(dirout)//'/'//trim(atmres)//'.mx'//trim(res)//trim(ctile)//'.nc'
     if(mastertask) then
       logmsg = 'creating mapped ocean mask file '//trim(fdst)
       print '(a)',trim(logmsg)
     end if

     dst2d(:,:) = dst3d(:,:,i)
     lat2d(:,:) = lat3d(:,:,i)
     lon2d(:,:) = lon3d(:,:,i)
   
     !rc = nf90_create(trim(fdst), nf90_64bit_offset, ncid)
     !rc = nf90_def_dim(ncid, 'grid_xt', npx, idimid)
     !rc = nf90_def_dim(ncid, 'grid_yt', npx, jdimid)

     !dim2(:) =  (/idimid, jdimid/)
     !vname = 'grid_xt'
     !rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
     !vname = 'grid_yt'
     !rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
     !!do nt = 1,ntra
     !!vname = 'land_frac'
     !!rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
     !!unit,long_name
     !!end do
     !rc = nf90_enddef(ncid)

     rc = nf90_open(trim(fdst), nf90_write, ncid)

     rc = nf90_inq_varid(ncid,    'grid_xt',      id)
     rc = nf90_put_var(ncid,             id,   lon2d)
     rc = nf90_inq_varid(ncid,    'grid_yt',      id)
     rc = nf90_put_var(ncid,             id,   lat2d)
     rc = nf90_inq_varid(ncid,  'land_frac',      id)
     rc = nf90_put_var(ncid,             id,   dst2d)
     rc = nf90_close(ncid)
    end do

!---------------------------------------------------------------------
! clean up
!---------------------------------------------------------------------

  deallocate(dst3d,lon3d,lat3d)
  deallocate(dst2d,lon2d,lat2d)
  !deallocate(col, row, S, lat1d, lon1d, src_field, dst_field)
#endif

  end subroutine make_tiled_data
end module mapped_data
