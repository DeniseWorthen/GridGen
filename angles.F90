module angles

  use grdvars, only : ni,nj,nx,ny
  use grdvars, only : x,y,xsgp1,ysgp1,sg_maxlat
  use grdvars, only : latBu,lonBu,lonCt
  use grdvars, only : angq,anglet
  use grdvars, only : debug

  contains

  subroutine find_angq

    implicit none
  
    ! local variables
    integer :: i,j,i1,i2,m,n
  
    ! pole locations on SG
    integer(kind=4) :: ipolesg(2)
  
    ! from geolonB fix in MOM6
    real(kind=8) :: len_lon ! The periodic range of longitudes, usually 360 degrees.
    real(kind=8) :: pi_720deg ! One quarter the conversion factor from degrees to radians.
    real(kind=8) :: lonB(2,2)  ! The longitude of a point, shifted to have about the same value.
    real(kind=8) :: lon_scale
  
    real(kind=8) :: modulo_around_point
!---------------------------------------------------------------------
! to find angleq on seam, replicate supergrid values across seam
!---------------------------------------------------------------------
  
    !pole on supergrid
    ipolesg = -1
        j = ny
    do i = 1,nx/2
     if(y(i,j) .eq. sg_maxlat)ipolesg(1) = i
    enddo
    do i = nx/2+1,nx
     if(y(i,j) .eq. sg_maxlat)ipolesg(2) = i
    enddo
    if(debug)print *,'poles found at ',ipolesg
  
    xsgp1(:,0:ny) = x(:,0:ny)
    ysgp1(:,0:ny) = y(:,0:ny)
    
    !check
    do i = ipolesg(1)-5,ipolesg(1)+5
     i2 = ipolesg(2)+(ipolesg(1)-i)+1
     if(debug)print *,i,i2
    enddo
     print *
    do i = ipolesg(2)-5,ipolesg(2)+5
     i2 = ipolesg(2)+(ipolesg(1)-i)+1
     if(debug)print *,i,i2
    enddo
  
    !replicate supergrid across pole
    do i = 1,nx
      i2 = ipolesg(2)+(ipolesg(1)-i)
     xsgp1(i,ny+1) = xsgp1(i2,ny)
     ysgp1(i,ny+1) = ysgp1(i2,ny)
    enddo
   
    !check
    if(debug)then
     j = ny+1
    i1 = ipolesg(1); i2 = ipolesg(2)-(ipolesg(1)-i1)
      print *,'replicate X across seam on SG'
      print *,xsgp1(i1-2,j),xsgp1(i2+2,j)
      print *,xsgp1(i1-1,j),xsgp1(i2+1,j)
      print *,xsgp1(i1,  j),xsgp1(i2,  j)
      print *,xsgp1(i1+1,j),xsgp1(i2-1,j)
      print *,xsgp1(i1+2,j),xsgp1(i2-2,j)
  
      print *,'replicate Y across seam on SG'
      print *,ysgp1(i1-2,j),ysgp1(i2+2,j)
      print *,ysgp1(i1-1,j),ysgp1(i2+1,j)
      print *,ysgp1(i1,  j),ysgp1(i2,  j)
      print *,ysgp1(i1+1,j),ysgp1(i2-1,j)
      print *,ysgp1(i1+2,j),ysgp1(i2-2,j)
    end if
  
!---------------------------------------------------------------------
! rotation angle on supergrid vertices
! lonB: x(i-1,j-1) has same relationship to x(i,j) on SG as
!       geolonT(i,j) has to geolonBu(i,j) on the reduced grid
!---------------------------------------------------------------------
  
     pi_720deg = atan(1.0) / 180.0
       len_lon = 360.0
    do j=1,ny ; do i=1,nx-1
      do n=1,2 ; do m=1,2
        lonB(m,n) = modulo_around_point(xsgp1(I+m-2,J+n-2), xsgp1(i-1,j-1), len_lon)
      enddo ; enddo
  
      lon_scale    = cos(pi_720deg*(ysgp1(i-1,j-1) + ysgp1(i+1,j-1) + &
                                    ysgp1(i-1,j+1) + ysgp1(i+1,j+1)) )
      angq(i,j)    = atan2(lon_scale*((lonB(1,2) - lonB(2,1)) + (lonB(2,2) - lonB(1,1))), &
                            ysgp1(i-1,j+1) + ysgp1(i+1,j+1) - &
                            ysgp1(i-1,j-1) - ysgp1(i+1,j-1) )
    enddo ; enddo
 
    !check
    if(debug) then
       j = ny
      i1 = ipolesg(1); i2 = ipolesg(2)-(ipolesg(1)-i1)
      print *,'angq along seam on SG'
      print *,angq(i1-2,j),angq(i2+2,j)
      print *,angq(i1-1,j),angq(i2+1,j)
      print *,angq(i1,  j),angq(i2,  j)
      print *,angq(i1+1,j),angq(i2-1,j)
      print *,angq(i1+2,j),angq(i2-2,j)
    end if

  end subroutine find_angq

  subroutine find_ang

    implicit none
  
    ! local variables
    integer :: i,j,m,n
    integer :: ii,jj
  
    ! from geolonB fix in MOM6
    real(kind=8) :: len_lon ! The periodic range of longitudes, usually 360 degrees.
    real(kind=8) :: pi_720deg ! One quarter the conversion factor from degrees to radians.
    real(kind=8) :: lonB(2,2)  ! The longitude of a point, shifted to have about the same value.
    real(kind=8) :: lon_scale
  
    real(kind=8) :: modulo_around_point
!---------------------------------------------------------------------
! rotation angle for "use_bugs" = false case from MOM6
! src/initialization/MOM_shared_initialization.F90 but allow for not
! having halo values
! note this does not reproduce sin_rot,cos_rot found in MOM6 output
! differences are ~O 10-6
!---------------------------------------------------------------------
  
     pi_720deg = atan(1.0) / 180.0
       len_lon = 360.0
      do j=1,nj; do i = 1,ni
        do n=1,2 ; do m=1,2
                      jj = J+n-2; ii = I+m-2
         if(jj .eq. 0)jj = 1
         if(ii .eq. 0)ii = ni
          lonB(m,n) = modulo_around_point(LonBu(ii,jj), LonCt(i,j), len_lon)
        !  lonB(m,n) = modulo_around_point(LonBu(I+m-2,J+n-2), LonCt(i,j), len_lon)
        enddo ; enddo
                      jj = j-1; ii = i-1
         if(jj .eq. 0)jj = 1
         if(ii .eq. 0)ii = ni
        lon_scale = cos(pi_720deg*((LatBu(ii,jj) + LatBu(I,J)) + &
                                   (LatBu(I,jj) + LatBu(ii,J)) ) )
       anglet(i,j) = atan2(lon_scale*((lonB(1,2) - lonB(2,1)) + (lonB(2,2) - lonB(1,1))), &
                      (LatBu(ii,J) - LatBu(I,jj)) + &
                      (LatBu(I,J) - LatBu(ii,jj)) )
                      
        !lon_scale = cos(pi_720deg*((LatBu(I-1,J-1) + LatBu(I,J)) + &
        !                           (LatBu(I,J-1) + LatBu(I-1,J)) ) )
       !anglet(i,j) = atan2(lon_scale*((lonB(1,2) - lonB(2,1)) + (lonB(2,2) - lonB(1,1))), &
       !               (LatBu(I-1,J) - LatBu(I,J-1)) + &
       !               (LatBu(I,J) - LatBu(I-1,J-1)) )
      enddo ; enddo

  end subroutine find_ang
end module angles

! -----------------------------------------------------------------------------
!> Return the modulo value of x in an interval [xc-(Lx/2) xc+(Lx/2)]
!! If Lx<=0, then it returns x without applying modulo arithmetic.
function modulo_around_point(x, xc, Lx) result(x_mod)
  real(kind=8), intent(in) :: x  !< Value to which to apply modulo arithmetic
  real(kind=8), intent(in) :: xc !< Center of modulo range
  real(kind=8), intent(in) :: Lx !< Modulo range width
  real(kind=8) :: x_mod          !< x shifted by an integer multiple of Lx to be close to xc.

  if (Lx > 0.0) then
    x_mod = modulo(x - (xc - 0.5*Lx), Lx) + (xc - 0.5*Lx)
  else
    x_mod = x
  endif
end function modulo_around_point
