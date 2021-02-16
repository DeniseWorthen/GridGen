module grdvars
 
  implicit none

  ! dimensions of output grid
  integer :: ni,nj
  ! dimensions of supergrid
  integer :: nx,ny

  ! edit mask flag
  logical :: editmask
  ! debug print flag
  logical :: debug

  !super-grid maximum latitude
     real(kind=8) :: sg_maxlat
  ! pole locations
  integer(kind=4) :: ipole(2)

  ! number of vertices
  integer, parameter :: nv = 4
  ! ij offsets moving counter-clockwise around each Ct(i,j)
  integer, parameter, dimension(nv) :: iVertCt = (/0, -1, -1,  0/)
  integer, parameter, dimension(nv) :: jVertCt = (/0,  0, -1, -1/)
  integer, dimension(nv) :: iVertBu, iVertCu, iVertCv
  integer, dimension(nv) :: jVertBu, jVertCu, jVertCv

  integer, parameter :: ncoord = 2*4             ! 4sets of lat/lon pairs
  integer, parameter :: nverts = 2*4             ! 4sets of lat/lon pairs vertices
  integer, parameter ::  nvars = ncoord + nverts

  ! super-grid source grid variables
  real(kind=8), allocatable, dimension(:,:)   :: x, y, angq
  real(kind=8), allocatable, dimension(:,:)   :: dx
  real(kind=8), allocatable, dimension(:,:)   :: dy
 
  !super-grid replicate row
  real(kind=8), allocatable, dimension(:,:) :: xsgp1, ysgp1
 
  ! grid stagger locations
  real(kind=8), allocatable, dimension(:,:) :: latCt, lonCt ! lat and lon of T on C-grid
  real(kind=8), allocatable, dimension(:,:) :: latCv, lonCv ! lat and lon of V on C-grid
  real(kind=8), allocatable, dimension(:,:) :: latCu, lonCu ! lat and lon of U on C-grid
  real(kind=8), allocatable, dimension(:,:) :: latBu, lonBu ! lat and lon of corners on C-grid

  ! areas of Ct grid cell
  real(kind=8), allocatable, dimension(:,:) :: areaCt
  ! rotation angle on Ct (opposite sense from angle)
  real(kind=8), allocatable, dimension(:,:) :: anglet
  ! rotation angle on Bu
  real(kind=8), allocatable, dimension(:,:) :: angle

  ! vertices of each stagger location
  real(kind=8), allocatable, dimension(:,:,:) :: latCt_vert, lonCt_vert
  real(kind=8), allocatable, dimension(:,:,:) :: latCu_vert, lonCu_vert
  real(kind=8), allocatable, dimension(:,:,:) :: latCv_vert, lonCv_vert
  real(kind=8), allocatable, dimension(:,:,:) :: latBu_vert, lonBu_vert

  ! need across seam values of Ct,Cu points to retrieve vertices of Bu and Cv grids
  real(kind=8), allocatable, dimension(:) :: xlonCt, xlatCt
  real(kind=8), allocatable, dimension(:) :: xlonCu, xlatCu
  ! latitude spacing at bottom of grid
  real(kind=8), allocatable, dimension(:) :: dlatBu, dlatCv

  ! ocean mask from fixed file, stored as either r4 or r8
     real(kind=4), allocatable, dimension(:,:) :: wet4
     real(kind=8), allocatable, dimension(:,:) :: wet8

  ! ice grid variables
  real(kind=8), allocatable, dimension(:,:) :: ulon, ulat
  real(kind=8), allocatable, dimension(:,:) ::  htn, hte

  contains

  subroutine allocate_all

  allocate( x(0:nx,0:ny),  y(0:nx,0:ny), angq(0:nx,0:ny) )
  allocate(  dx(nx,0:ny), dy(0:nx,ny) )

  allocate( xsgp1(0:nx,0:ny+1), ysgp1(0:nx,0:ny+1) )

  allocate( latCt(ni,nj), lonCt(ni,nj) )
  allocate( latCv(ni,nj), lonCv(ni,nj) )
  allocate( latCu(ni,nj), lonCu(ni,nj) )
  allocate( latBu(ni,nj), lonBu(ni,nj) )

  allocate( areaCt(ni,nj), anglet(ni,nj), angle(ni,nj) )

  allocate( latCt_vert(ni,nj,nv), lonCt_vert(ni,nj,nv) )
  allocate( latCv_vert(ni,nj,nv), lonCv_vert(ni,nj,nv) )
  allocate( latCu_vert(ni,nj,nv), lonCu_vert(ni,nj,nv) )
  allocate( latBu_vert(ni,nj,nv), lonBu_vert(ni,nj,nv) )

  allocate( xlonCt(ni), xlatCt(ni) )
  allocate( xlonCu(ni), xlatCu(ni) )
  allocate( dlatBu(ni), dlatCv(ni) )

     allocate( wet4(ni,nj) )
     allocate( wet8(ni,nj) )

  allocate( ulon(ni,nj), ulat(ni,nj) )
  allocate(  htn(ni,nj),  hte(ni,nj) )

  end subroutine allocate_all
  
end module grdvars
