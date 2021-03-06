program gen_fixgrid
!
! Denise.Worthen@noaa.gov
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This code generates files the fixed grid and land mask file for the CICE
! model on the MOM6 tripole grid. It also generates a fixed grid file which
! contains all the vertice locations on the tripole grid. This fixed grid
! file is used to create the interpolation weights for regridding between
! various combinations of tripole and rectilinear grids.
!
! information on MOM6 supergrid can be found at
! https://gist.github.com/adcroft/c1e207024fe1189b43dddc5f1fe7dd6c
!
! also: https://mom6.readthedocs.io/en/latest/api/generated/modules/mom_grid.html
!
! also:
! MOM_grid_initialize.F90 :
!  MOM6 variable geoLonBu <==> CICE variable ulon
!  MOM6 variable geoLatBu <==> CICE variable ulat
!  MOM6 variable     dxCv <==> CICE variable htn
!  MOM6 variable     dyCu <==> CICE variable hte
!
! MOM6 code snippets follow:
!
! from MOM_grid_initialize.F90  (tmpZ = x)
!  do J=G%JsdB,G%JedB ; do I=G%IsdB,G%IedB ; i2 = 2*I ; j2 = 2*J
!    G%geoLonBu(I,J) = tmpZ(i2,j2)
! so....
!          ulon(I,J) = x(i2,j2)

! from MOM_grid_initialize.F90  (tmpZ = y)
!  do J=G%JsdB,G%JedB ; do I=G%IsdB,G%IedB ; i2 = 2*I ; j2 = 2*J
!    G%geoLatBu(I,J) = tmpZ(i2,j2)
! so....
!          ulat(I,J) = y(i2,j2)

! from MOM_grid_initialize.F90  (tmpV = dx)
!  do J=G%JsdB,G%JedB ; do i=G%isd,G%ied ; i2 = 2*i ; j2 = 2*j
!    dxCv(i,J) = tmpV(i2-1,j2) + tmpV(i2,j2)
! so....
!     htn(i,J) =   dx(i2-1,j2) +   dx(i2,j2)

! from MOM_grid_initialize.F90  (tmpU = dy)
!  do J=G%JsdB,G%JedB ; do i=G%isd,G%ied ; i2 = 2*i ; j2 = 2*j
!    dyCu(I,j) = tmpU(i2,j2-1) + tmpU(i2,j2)
! so....
!     hte(I,j) =   dy(i2,j2-1) +   dy(i2,j2)
!
! rotation angle on supergrid vertices can be found
! using the formula in MOM_shared_initialization.F90, accounting
! for indexing difference between reduced grid and super grid
!
!
!         SuperGrid                 Reduced grid
!
!  i-1,j+1         i+1,j+1
!     X-------X-------X             I-1,J      I,J
!     |       |       |                X-------X
!     |       |       |                |       |
!     |       | i,j   |                |   T   |
!     X-------X-------X                |       |
!     |       |       |                X-------X
!     |       |       |             I-1,J-1   I,J-1
!     |       |       |
!     X-------X-------X
!  i-1,j-1         i+1,j-1
!
! so that in angle formulae
!         I==>i+1,I-1==>i-1
!         J==>j+1,J-1==>j-1
!
! CICE expects angle to be XY -> LatLon so change the sign from MOM6 value
! This has been determined from the HYCOM code: ALL/cice/src/grid2cice.f
!
!            anglet(i,j) =    -pang(i+i0,  j+j0)   !radians
!c           pang is from lon-lat to x-y, but anglet is the reverse
!
! where anglet is the angle variable being written to the CICE grid file
! and pang is HYCOM's own rotation angle.
!
! Area of the T-grid cell is obtained as in MOM_grid_initialize where
! tmpV = dx on SG and tmpU is dy on SG
!
!    dxT(i,j) = tmpV(i2-1,j2-1) + tmpV(i2,j2-1)
!    dyT(i,j) = tmpU(i2-1,j2-1) + tmpU(i2-1,j2)
!
! This code utilizes a "seam flip" to obtain the required values across
! the tripole seam. If ipL an ipR are the i-indices of the pole along the
! last j-row of the reduced grid, then:
!
! ipL-1     ipL    ipL+1       ipR-1     ipR    ipR+1
!    x-------x-------x     |||    x-------x-------x
!
! Fold over; ipL must align with ipR
!
!  ipR+1     ipR    ipR-1
!     x-------x-------x
!  ipL-1     ipL    ipL+1
!     x-------x-------x
!
!
! SCRIP requires that the vertices be ordered counter-clockwise so that
! the center grid point is always to the left of the vertex. Here,
! Vertices are defined counter-clockwise from upper right. Ct-grid vertices
! are located on the Bu grid; Cu vertices on the Cv grid, Cv vertices on the Cu
! grid and Bu vertices on the Ct grid. For example, for the Ct-grid, the vertices
! are:
!             Vertex #2             Vertex #1
!             Bu(i-1,j)             Bu(i,j)
!                         Ct(i,j)
!           Bu(i-1,j-1)             Bu(i,j-1)
!             Vertex #3             Vertex #4
!
! so that the vertices of any Ct(i,j) are found as off-sets of the i,j index on the
! Bu grid
!
!     iVertCt(4) = (/0, -1, -1, 0/)
!     jVertCt(4) = (/0, 0, -1, -1/)
!
! Careful examination of the Cu,Cv and Bu grids lead to similar definitions for the
! i,j offsets required to extract the other grid stragger vertices locations, all of
! which can be defined in terms of the iVertCt and jVertCt values
!
! Special treatment is require at the bottom of the grid, where the verticies of the
! Ctand Cu grid must be set manually (note, these points are on land.) The top of
! the grid also requires special treatment because the required verticies are located
! across the tripole seam. This is accomplished by creating 1-d arrays which hold
! the Ct and Cu grid point locations across the matched seam.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use netcdf
  use param
  use grdvars
  use angles
  use physcon
  use charstrings
  use debugprint
  use fixgriddefs

  implicit none

  real(kind=8) :: dxT, dyT

  character(len=256) :: fname_out, fname_in
  character(len=300) :: cmdstr

  integer :: rc,ncid,id,xtype
  integer :: i,j,i2,j2
  integer :: ii,jj,k
  integer :: system

!---------------------------------------------------------------------
! set up the arrays to retrieve the vertices
!---------------------------------------------------------------------

  iVertCu = iVertCt + 1; jVertCu = jVertCt + 0
  iVertCv = iVertCt + 0; jVertCv = jVertCt + 1
  iVertBu = iVertCt + 1; jVertBu = jVertCt + 1

  print '(a8,4i6)','iVertCt ',(iVertCt(i),i=1,4)
  print '(a8,4i6)','jVertCt ',(jVertCt(i),i=1,4)
  print *
  print '(a8,4i6)','iVertCu ',(iVertCu(i),i=1,4)
  print '(a8,4i6)','jVertCu ',(jVertCu(i),i=1,4)
  print *
  print '(a8,4i6)','iVertCv ',(iVertCv(i),i=1,4)
  print '(a8,4i6)','jVertCv ',(jVertCv(i),i=1,4)
  print *
  print '(a8,4i6)','iVertBu ',(iVertBu(i),i=1,4)
  print '(a8,4i6)','jVertBu ',(jVertBu(i),i=1,4)
  print *

  latCt_vert = -9999.0 ; lonCt_vert = -9999.0
  latCu_vert = -9999.0 ; lonCu_vert = -9999.0
  latCv_vert = -9999.0 ; lonCv_vert = -9999.0
  latBu_vert = -9999.0 ; lonBu_vert = -9999.0

!---------------------------------------------------------------------
! read the land mask
!---------------------------------------------------------------------

  fname_in = trim(dirsrc)//trim(res)//'/'//trim(maskfile)

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *, 'reading ocean mask from ',trim(fname_in)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  trim(maskname), id)
  rc = nf90_inquire_variable(ncid, id, xtype=xtype)
  if(xtype .eq. 5)rc = nf90_get_var(ncid,      id,  wet4)
  if(xtype .eq. 6)rc = nf90_get_var(ncid,      id,  wet8)
  rc = nf90_close(ncid)

  if(xtype.eq. 6)wet4 = real(wet8,4)

  print *,minval(wet8),maxval(wet8)
  print *,minval(wet4),maxval(wet4)
#ifdef output_grid_1deg
!---------------------------------------------------------------------
! kludgy fix: 1-deg model has single point which switches froma
! land->ocean at run time. see issue #47 on NOAA-EMC/MOM6
!---------------------------------------------------------------------

   ii = 88; jj = 132
   if(wet4(ii+1,jj+1) .eq. 0.0)wet4(ii+1,jj+1) = 1.0
#endif
!---------------------------------------------------------------------
! read supergrid file
!---------------------------------------------------------------------

  fname_in = trim(dirsrc)//trim(res)//'/'//'ocean_hgrid.nc'

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *, 'reading supergrid from ',trim(fname_in)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))
  
  rc = nf90_inq_varid(ncid, 'x', id)  !lon
  rc = nf90_get_var(ncid,    id,  x)
 
  rc = nf90_inq_varid(ncid, 'y', id)  !lat
  rc = nf90_get_var(ncid,    id,  y)

  rc = nf90_inq_varid(ncid, 'dx', id)
  rc = nf90_get_var(ncid,     id, dx)

  rc = nf90_inq_varid(ncid, 'dy', id)
  rc = nf90_get_var(ncid,     id, dy)

  rc = nf90_close(ncid)
  print *,'super grid size ',size(y,1),size(y,2)
  print *,'max lat in super grid ',maxval(y)
  sg_maxlat = maxval(y)

!---------------------------------------------------------------------
! find the angle on corners---this requires the supergrid
!---------------------------------------------------------------------

    call find_angq

!---------------------------------------------------------------------
! fill grid variables
!---------------------------------------------------------------------

  do j = 1,nj
   do i = 1,ni
     i2 = 2*i ; j2 = 2*j
    !deg->rad
      ulon(i,j) =     x(i2,j2)*deg2rad
      ulat(i,j) =     y(i2,j2)*deg2rad
    !in rad already
     angle(i,j) = -angq(i2,j2)
    !m->cm
       htn(i,j) = (dx(i2-1,j2) + dx(i2,j2))*100.0
       hte(i,j) = (dy(i2,j2-1) + dy(i2,j2))*100.0
    !deg
     lonBu(i,j) =     x(i2,j2)
     latBu(i,j) =     y(i2,j2)
    !deg
     lonCt(i,j) =     x(i2-1,j2-1)
     lonCu(i,j) =     x(i2,  j2-1)
     lonCv(i,j) =     x(i2-1,j2  )
    !deg
     latCt(i,j) =     y(i2-1,j2-1)
     latCu(i,j) =     y(i2,  j2-1)
     latCv(i,j) =     y(i2-1,j2  )
    !m2
            dxT = dx(i2-1,j2-1) + dx(i2,j2-1)
            dyT = dy(i2-1,j2-1) + dy(i2-1,j2)
    areaCt(i,j) = dxT*dyT
   enddo
  enddo

!---------------------------------------------------------------------
! find the angle on centers---this does not requires the supergrid
!---------------------------------------------------------------------

    call find_ang

    print *,'ANGLET ',minval(anglet),maxval(anglet)
    print *,'ANGLE  ',minval(angle),maxval(angle)

!---------------------------------------------------------------------
! For the 1/4deg grid, hte at j=720 and j = 1440 is identically=0.0 for
! j > 840 (64.0N). These are land points, but since CICE uses hte to
! generate remaining variables, setting them to zero will cause problems
! For 1deg grid, hte at ni/2 and ni are very small O~10-12, so test for
! hte < 1.0
!---------------------------------------------------------------------

   print *,'min vals of hte at folds ',minval(hte(ni/2,:)),minval(hte(ni,:))
   do j = 1,nj
      ii = ni/2
    if(hte(ii,j) .le. 1.0)hte(ii,j) = 0.5*(hte(ii-1,j) + hte(ii+1,j))
      ii = ni
    if(hte(ii,j) .le. 1.0)hte(ii,j) = 0.5*(hte(ii-1,j) + hte(   1,j))
   enddo
   print *,'min vals of hte at folds ',minval(hte(ni/2,:)),minval(hte(ni,:))

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

  where(lonCt .lt. 0.0)lonCt = lonCt + 360.d0
  where(lonCu .lt. 0.0)lonCu = lonCu + 360.d0
  where(lonCv .lt. 0.0)lonCv = lonCv + 360.d0
  where(lonBu .lt. 0.0)lonBu = lonBu + 360.d0

!---------------------------------------------------------------------
! some basic error checking
! find the i-th index of the poles at j= nj
! the corner points must lie on the pole
!---------------------------------------------------------------------

  ipole = -1
      j = nj
  do i = 1,ni/2
   if(latBu(i,j) .eq. sg_maxlat)ipole(1) = i
  enddo
  do i = ni/2+1,ni
   if(latBu(i,j) .eq. sg_maxlat)ipole(2) = i
  enddo
  print *,'poles found at ',ipole,latBu(ipole(1),nj),latBu(ipole(2),nj)

  call checkseam

  do i = 1,ni
    i2 = ipole(2)+(ipole(1)-i)+1
    xlonCt(i) = lonCt(i2,nj)
    xlatCt(i) = latCt(i2,nj)
  enddo

  !do i = 1,10
  !  i2 = ipole(2)+(ipole(1)-i)
  !  print *,i,i2,lonCu(i,nj)
  !enddo
  !do i = 1430,1440
  !  i2 = ipole(2)+(ipole(1)-i)
  !  if(i2 .lt. 1)i2 = ni
  !  print *,i,i2,lonCu(i,nj)
  !enddo
 
  do i = 1,ni
    i2 = ipole(2)+(ipole(1)-i)
    if(i2 .lt. 1)i2 = ni
   xlonCu(i) = lonCu(i2,nj)
   xlatCu(i) = latCu(i2,nj)
   !print *,i,xlonCu(i),lonCu(i2,nj)
  enddo
 
  call checkxlatlon
  !do i = 1,ni
  !  i2 = ipole(2)+(ipole(1)-i)
  !  if(i2 .lt. 1)i2 = ni
  ! print *,i,xlonCu(i),lonCu(i2,nj)
  !enddo

  !approx lat at grid bottom
  do i = 1,ni
   dlatBu(i) = latBu(i,1) + 2.0*(latCu(i,1) - latBu(i,1))
   dlatCv(i) = latCt(i,1) + 2.0*(latCt(i,1) - latCv(i,1))
  enddo

!---------------------------------------------------------------------
! fill grid vertices variables
!---------------------------------------------------------------------

  !Ct and Cu grids align in j
  call fill_vertices(2,nj  , iVertCt,jVertCt, latBu,lonBu, latCt_vert,lonCt_vert)
  call           fill_bottom(iVertCt,jVertCt, latBu,lonBu, latCt_vert,lonCt_vert,dlatBu)

  call fill_vertices(2,nj  , iVertCu,jVertCu, latCv,lonCv, latCu_vert,lonCu_vert)
  call           fill_bottom(iVertCu,jVertCu, latCv,lonCv, latCu_vert,lonCu_vert,dlatCv)

  !Cv and Bu grids align in j
  call fill_vertices(1,nj-1, iVertCv,jVertCv, latCu,lonCu, latCv_vert,lonCv_vert)
  call              fill_top(iVertCv,jVertCv, latCu,lonCu, latCv_vert,lonCv_vert, xlatCu, xlonCu)

  call fill_vertices(1,nj-1, iVertBu,jVertBu, latCt,lonCt, latBu_vert,lonBu_vert)
  call              fill_top(iVertBu,jVertBu, latCt,lonCt, latBu_vert,lonBu_vert, xlatCt, xlonCt)
 
  call checkpoint

  if(minval(latCt_vert) .lt. -1.e3)stop
  if(minval(lonCt_vert) .lt. -1.e3)stop
  if(minval(latCu_vert) .lt. -1.e3)stop
  if(minval(lonCu_vert) .lt. -1.e3)stop
  if(minval(latCv_vert) .lt. -1.e3)stop
  if(minval(lonCv_vert) .lt. -1.e3)stop
  if(minval(latBu_vert) .lt. -1.e3)stop
  if(minval(lonBu_vert) .lt. -1.e3)stop

!---------------------------------------------------------------------
! write out grid file files
!---------------------------------------------------------------------

  ! create a history attribute
   call date_and_time(date=cdate)
   history = 'created on '//trim(cdate)//' from '//trim(fname_in)

   call write_tripolegrid

   call write_cicegrid

!---------------------------------------------------------------------
! extract the kmt into a separate file
!---------------------------------------------------------------------

   fname_in =  trim(dirout)//'grid_cice_NEMS_mx'//trim(res)//'.nc'
  fname_out = trim(dirout)//'kmtu_cice_NEMS_mx'//trim(res)//'.nc'

     cmdstr = 'ncks -O -v kmt '//trim(fname_in)//'  '//trim(fname_out)
     rc = system(trim(cmdstr))

end program gen_fixgrid
