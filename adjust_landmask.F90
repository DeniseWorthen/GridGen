subroutine adjust_landmask

   use param
   use grdvars

   implicit none

  ! local variables
  integer :: i,j,im1,ip1

  real(kind=4), dimension(ni,nj) :: kmtsum

  xwet = wet4

!---------------------------------------------------------------------
! set j=1 to land
!---------------------------------------------------------------------

   xwet(:,1) = 0.0

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

   kmtsum = 0.0
  do j = 2,nj-1
   do i = 1,ni
    if ((wet4(i,j) .eq. 1.0) .and. &
         (latCt(i,j) .le. -60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     kmtsum(i,j) = wet4(im1,j) + wet4(ip1,j) + wet4(i,j-1) + wet4(i,j+1)
    end if
   end do
  end do

  do j = 2,nj
   do i = 1,ni
    if ((wet4(i,j) .eq. 1.0) .and. &
         (latCt(i,j) .le. -60.0)) then
      if(kmtsum(i,j) .le. 1.0)xwet(i,j) = 0.0
      if(kmtsum(i,j) .le. 1.0)print *,i,j,kmtsum(i,j),wet4(i,j),xwet(i,j)
    endif
   enddo
  enddo

  wet4 = xwet

end subroutine adjust_landmask
