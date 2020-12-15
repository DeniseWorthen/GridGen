module grdvars

  use param
 
  implicit none

  ! super-grid source variables
  real(kind=8), dimension(0:nx,0:ny)   :: x, y, angq
  real(kind=8), dimension(  nx,0:ny)   :: dx
  real(kind=8), dimension(0:nx,  ny)   :: dy
 
  !super-grid replicate row
  real(kind=8), dimension(0:nx,0:ny+1) :: xsgp1, ysgp1
 
  !super-grid maximum latitude
     real(kind=8) :: sg_maxlat
  ! pole locations
  integer(kind=4) :: ipole(2)

  ! grid stagger locations
  real(kind=8), dimension(ni,nj) :: latCt, lonCt ! lat and lon of T on C-grid
  real(kind=8), dimension(ni,nj) :: latCv, lonCv ! lat and lon of V on C-grid
  real(kind=8), dimension(ni,nj) :: latCu, lonCu ! lat and lon of U on C-grid
  real(kind=8), dimension(ni,nj) :: latBu, lonBu ! lat and lon of corners on C-grid

  ! areas of Ct grid cell
  real(kind=8), dimension(ni,nj) :: areaCt
  ! rotation angle on Ct (opposite sense from angle)
  real(kind=8), dimension(ni,nj) :: anglet
  ! rotation angle on Bu
  real(kind=8), dimension(ni,nj) :: angle

  ! vertices of each stagger location
  real(kind=8), dimension(ni,nj,nv) :: latCt_vert, lonCt_vert
  real(kind=8), dimension(ni,nj,nv) :: latCu_vert, lonCu_vert
  real(kind=8), dimension(ni,nj,nv) :: latCv_vert, lonCv_vert
  real(kind=8), dimension(ni,nj,nv) :: latBu_vert, lonBu_vert

  integer, dimension(nv) :: iVertBu, iVertCu, iVertCv
  integer, dimension(nv) :: jVertBu, jVertCu, jVertCv

  ! need across seam values of Ct,Cu points to retrieve vertices of Bu and Cv grids
  real(kind=8), dimension(ni) :: xlonCt, xlatCt
  real(kind=8), dimension(ni) :: xlonCu, xlatCu
  ! latitude spacing at bottom of grid
  real(kind=8), dimension(ni) :: dlatBu, dlatCv

  ! ocean mask from fixed file, stored as either r4 or r8
     real(kind=4), dimension(ni,nj) :: wet4
     real(kind=8), dimension(ni,nj) :: wet8

  ! ice grid variables
  real(kind=8), dimension(ni,nj) :: ulon, ulat
  real(kind=8), dimension(ni,nj) ::  htn, hte

end module grdvars
