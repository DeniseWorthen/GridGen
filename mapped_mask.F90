module mapped_mask

  use gengrid_kinds, only : dbl_kind, int_kind
  use grdvars,       only : ni,nj,nx,ny
  use charstrings
  use netcdf

  implicit none

  contains

  subroutine make_frac_land(src, wgt)

  character(len=*), intent(in) :: src, wgt

  integer, parameter :: ntile = 6
  integer, parameter :: nres = 48
  ! local variables
  integer(int_kind) :: n_a, n_b, n_s

  integer(int_kind), allocatable, dimension(:) :: col, row
     real(dbl_kind), allocatable, dimension(:) :: S
     real(dbl_kind), allocatable, dimension(:) :: lat1d, lon1d

  integer(int_kind), allocatable, dimension(:) :: src_field
     real(dbl_kind), allocatable, dimension(:) :: dst_field

     real(dbl_kind), allocatable, dimension(:,:,:) :: dst3d

  character(len=CL) :: fname_out
  integer :: i,ii,jj,id,rc,ncid, dim2(2),dim3(3)
  integer :: istr,iend
  integer :: idimid,jdimid,kdimid

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

!---------------------------------------------------------------------
! retrieve 1-d land mask from the SCRIP file and map it
!---------------------------------------------------------------------

    allocate(src_field(1:n_b))
    allocate(dst_field(1:n_b))

    rc = nf90_open(trim(src), nf90_nowrite, ncid)

    !1-d ocean mask (integer)
    rc = nf90_inq_varid(ncid, 'grid_imask', id)
    rc = nf90_get_var(ncid,     id,  src_field)
    rc = nf90_close(ncid)

    do i = 1,n_s
      ii = row(i); jj = col(i)
      dst_field(ii) = dst_field(ii) + S(i)*real(src_field(jj),8)
    enddo

    allocate(dst3d(nres,nres,ntile))
    do i = 1,ntile
     istr = i*nres*nres+1
     iend = istr+nres*nres
     dst3d(:,:,i) = reshape(dst_field(istr:iend), (/nres,nres/))
    end do

!---------------------------------------------------------------------
! clean up
!---------------------------------------------------------------------

  deallocate(col, row, S, lat1d, lon1d, src_field, dst_field)

  end subroutine make_frac_land
end module mapped_mask
