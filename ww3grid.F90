module ww3grid

  use gengrid_kinds, only: dbl_kind,real_kind,int_kind,CL,CS
  use grdvars,       only: ni,nj,nv,mastertask
  use charstrings,   only: dirout,res,logmsg,staggerlocs
  use scripgrid,     only: write_scripgrid

  implicit none
  private

  public ww3files

  interface flip
    module procedure flip2dr8
    module procedure flip3dr8
    module procedure flip2di4
  end interface

  ! local variables
  ! grid 4 = total grid
  integer, parameter :: ngrids = 4, joverlap = 2
  integer, dimension(ngrids) :: jbeg, jend
  integer(int_kind) :: ipoles(2)

  character(len= 4), dimension(ngrids) :: grdnms = (/'.g01','.g02', '.g03','    '/)
  character(len= 6) :: i4fmt = '(i4.4)'
  character(len=CS) :: form1
  character(len=CS) :: form2
  character(len= 6) :: clen

  contains

!---------------------------------------------------------------------
  subroutine ww3files

   use grdvars,       only: lonCt,latCt,lonCt_vert,latCt_vert
   use grdvars,       only: wet4, dp4
   use grdvars,       only: maximum_lat

   ! local variables
   integer :: ng,jj,i,j,lx,ly
   integer :: ibeg,iend,i2

   character(len=CL) :: fdst
   character(len=CS) :: cstagger, gname
   real(dbl_kind)   , allocatable, dimension(:,:)   :: lats, lons, depths
   real(dbl_kind)   , allocatable, dimension(:,:,:) :: latverts, lonverts
   integer(int_kind), allocatable, dimension(:,:)   :: mask

   integer(int_kind), dimension(ni,nj,ngrids)   :: imask

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

   do ng = 1,ngrids
      imask(:,:,ng) = int(wet4(:,:))
   end do
   do ng = 1,ngrids
      do j = 1,nj
         do i = 1,ni
            if (imask(i,j,ng) == 1) then
              if (latCt(i,j) .ge. maximum_lat) imask(i,j,ng) = 3
            end if
         end do
      end do
      where (imask(:,nj,ng) == 1) imask(:,nj,ng) = 3
   end do

   call define_ww3grids(ipoles,imask)

   ! Ct grid only
   cstagger = trim(staggerlocs(1))
   do ng = 1,ngrids

      lx = ni
      ly = (jend(ng) - jbeg(ng))+1
      ! polar grid
      !if (ng .eq. 3)then
      !   lx = ni/2
      !   ly = 2*ly
      !end if
      print '(3(a,i4))','grid = ',ng,' lx = ',lx,' ly = ',ly
    
      ! allocate arrays for this grid
      allocate(    lats(1:lx,1:ly))
      allocate(    lons(1:lx,1:ly))
      allocate(latverts(1:lx,1:ly,1:nv))
      allocate(lonverts(1:lx,1:ly,1:nv))
      allocate(  depths(1:lx,1:ly))
      allocate(    mask(1:lx,1:ly))
      lats   = -999.; lons = -999.; latverts = -999.; lonverts = -999.
      depths = -999.; mask = -999

      !if (ng == 3) then
      !   ibeg = 1; iend = 2*ipoles(1)
      !else
         ibeg = 1; iend = lx
      !end if
  
      gname = trim(grdnms(ng))
        jj = 0
      do j = jbeg(ng),jend(ng)
        jj = jj+1
            lats(ibeg:iend,jj)   =      latCt(ibeg:iend,j)
            lons(ibeg:iend,jj)   =      lonCt(ibeg:iend,j)
          depths(ibeg:iend,jj)   =        dp4(ibeg:iend,j)
            mask(ibeg:iend,jj)   =      imask(ibeg:iend,j,ng)
        latverts(ibeg:iend,jj,:) = latCt_vert(ibeg:iend,j,:)
        lonverts(ibeg:iend,jj,:) = lonCt_vert(ibeg:iend,j,:)
      end do
#ifdef test
      ! fill in 2nd half for 3rd (polar) grid
      if (ng == 3) then
          jj = (jend(ng) - jbeg(ng))+1
        do j = jend(ng),jbeg(ng),-1
          jj = jj+1
          do i = 1,2*ipoles(1)
               i2 = ipoles(2)+(ipoles(1)-i)+1
                 lats(i,jj)   =      latCt(i2,j)
                 lons(i,jj)   =      lonCt(i2,j)
                 mask(i,jj)   =      imask(i2,j,ng)
               depths(i,jj)   =        dp4(i2,j)
             latverts(i,jj,:) = latCt_vert(i2,j,:)
             lonverts(i,jj,:) = lonCt_vert(i2,j,:)
          end do
        end do
        ! flip
        call flip(lx,ly,    lats)
        call flip(lx,ly,    lons)
        call flip(lx,ly,  depths)
        call flip(lx,ly,latverts)
        call flip(lx,ly,lonverts)
        call flip(lx,ly,    mask)
      endif
#endif
      write(clen,i4fmt)lx
      write(form1,'(a)')'('//trim(clen)//'f8.2)'
      write(form2,'(a)')'('//trim(clen)//'i2)'
      do j = 1,ly
       write(ng*10+1,trim(form1))(lats(i,j),i=1,lx)
       write(ng*10+2,trim(form1))(lons(i,j),i=1,lx)
       write(ng*10+3,trim(form1))(real(mask(i,j),4),i=1,lx)
       write(ng*10+4,trim(form1))(depths(i,j),i=1,lx)
      end do

      ! don't write SCRIP for total grid
      if (ng /= 4) then
         fdst= trim(dirout)//'/'//trim(cstagger)//'.mx'//trim(res)//trim(gname)//'_SCRIP_land.nc'
         call write_scripgrid(fdst,lx,ly,lats,lons,latverts,lonverts,mask)
         if (mastertask) then
           logmsg = 'creating SCRIP file '//trim(fdst)
           print '(a)',trim(logmsg)
         end if
      end if
      call ww3_asciifiles(gname,lx,ly,lats,lons,depths,mask)

      deallocate(lats, lons, latverts, lonverts, depths, mask)
   end do

  end subroutine ww3files


!---------------------------------------------------------------------
  subroutine ww3_asciifiles(gname,lx,ly,lats,lons,depths,mask)

    use grdvars,       only: maximum_lat

    integer(int_kind), intent(in) :: lx, ly
    character(len=*) , intent(in) :: gname
    real(dbl_kind)   , intent(in), dimension(lx,ly) :: lats, lons, depths
    integer(int_kind), intent(in), dimension(lx,ly) :: mask

    ! local variables
    integer :: i,j

!---------------------------------------------------------------------
! write lat,lon,depth and mask arrays required by ww3 in creating
! mod_def file
! dp4 has already been adjusted by minimum_depth
!---------------------------------------------------------------------

    write(clen,i4fmt)lx
    write(form1,'(a)')'('//trim(clen)//'f14.8)'
    write(form2,'(a)')'('//trim(clen)//'i2)'

    print *,trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_x.inp'
    open(unit=21,file=trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_x.inp',form='formatted')
    open(unit=22,file=trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_y.inp',form='formatted')
    open(unit=23,file=trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_bottom.inp',form='formatted')
    open(unit=24,file=trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_mapsta.inp',form='formatted')
    ! cice0 .ne. cicen requires obstruction map, should be initialized as zeros (w3grid,ln3032)
    open(unit=25,file=trim(dirout)//'/'//'ww3.mx'//trim(res)//trim(gname)//'_obstr.inp',form='formatted')

    do j = 1,ly
     write( 21,trim(form1))lons(:,j)
     write( 22,trim(form1))lats(:,j)
     write( 23,trim(form1))depths(:,j)
     write( 24,trim(form2))mask(:,j)
     !'obsx' and 'obsy' arrays ???
     write( 25,trim(form2))mask(:,j)*0
     write( 25,trim(form2))mask(:,j)*0
    end do
    close(21); close(22); close(23); close(24); close(25)

   end subroutine ww3_asciifiles

!---------------------------------------------------------------------
  subroutine define_ww3grids(ipoles,imask)

   use grdvars,       only: lonCt,latCt
   use grdvars,       only: lonBu,latBu

   integer(int_kind), intent(out)   :: ipoles(2)
   integer(int_kind), intent(inout) :: imask(ni,nj,ngrids)

   ! local variables
   integer :: ng,i,j,j1,j2

!---------------------------------------------------------------------
! split grid in 3 and 50S, 50N and write files used for testing multi
! each will get written as a SCRIP file in order to create a mesh file
! and also as ascii files in order to create mod_defs. The last grid is
! the full grid
!---------------------------------------------------------------------

   ipoles = -1
        j = nj
     do i = 1,ni/2
      if(latBu(i,j) .eq. 90.0)ipoles(1) = i
     enddo
     do i = ni/2+1,ni
      if(latBu(i,j) .eq. 90.0)ipoles(2) = i
     enddo
     if(mastertask)print *,'poles for WW3 found at ',ipoles

     jbeg = -1
     jend = -1
     i = ipoles(1)

     jbeg(1) = 1
     ! first sub grid
     do j = nj,1,-1
      if(latCt(i,j) .ge. -50.)jend(1) = j
      !print *,j,jend(1),latCt(i,j)
     end do
     jend(1) = jend(1) + joverlap
 
     ! second grid
     ! overlap rows
     jbeg(2) = jend(1) - joverlap
     do j = 1,nj
      if(latCt(i,j) .le.  50.)jend(2) = j
      !print *,j,jend(2),latCt(i,j)
     end do
     jend(2) = jend(2) + joverlap

     ! the third (polar) grid. this one may need special treatment
     jbeg(3) = jend(2) - joverlap
     jend(3) = nj

     ! the total grid
     jbeg(4) = 1
     jend(4) = nj

     do ng = 1,ngrids
      j1 = jbeg(ng); j2 = jend(ng)
      print '(a,4i5,2f8.2)','grid= ',ng,j1,j2,(j2-j1)+1,latCt(i,j1),latCt(i,j2)
     end do

     ! set up masking
     ng = 1
     imask(:,jend(ng)-1,ng) = 2
     imask(:,jend(ng)  ,ng) = 3

     ng = 2
     imask(:,jbeg(ng)  ,ng) = 3
     imask(:,jbeg(ng)+1,ng) = 2
     imask(:,jend(ng)-1,ng) = 2
     imask(:,jend(ng)  ,ng) = 3
     
     ng = 3
     imask(:,jbeg(ng)  ,ng) = 3
     imask(:,jbeg(ng)+1,ng) = 2
 
     i = 100
     do j = 1,nj
      if(imask(i,j,4) > 0)print '(5i4)',j,imask(i,j,1),imask(i,j,2),imask(i,j,3),imask(i,j,4)
     end do
  end subroutine define_ww3grids

!---------------------------------------------------------------------
  subroutine flip2dr8(lx,ly,ain)

    integer, intent(in) :: lx,ly

    real(dbl_kind), dimension(lx,ly), intent(inout) :: ain
    !local variables
    integer :: i,j,ii,jj
    real(dbl_kind), dimension(lx,ly)    :: atmp

    atmp = ain
    do j = 1,ly
      jj = (ly-j)+1
     do i = 1,lx
       ii = (lx-i)+1
      ain(ii,jj) = atmp(i,j)
     end do
    end do
  end subroutine flip2dr8

!---------------------------------------------------------------------
  subroutine flip3dr8(lx,ly,ain)

    use grdvars,       only: nv

    integer, intent(in) :: lx,ly

    real(dbl_kind), dimension(lx,ly,nv), intent(inout) :: ain
    !local variables
    integer :: i,j,ii,jj
    real(dbl_kind), dimension(lx,ly,nv)    :: atmp

    atmp = ain
    do j = 1,ly
      jj = (ly-j)+1
     do i = 1,lx
       ii = (lx-i)+1
      ain(ii,jj,:) = atmp(i,j,:)
     end do
    end do
  end subroutine flip3dr8

!---------------------------------------------------------------------
  subroutine flip2di4(lx,ly,ain)

    integer, intent(in) :: lx,ly

    integer(int_kind), dimension(lx,ly), intent(inout) :: ain
    !local variables
    integer :: i,j,ii,jj
    integer(int_kind), dimension(lx,ly)    :: atmp

    atmp = ain
    do j = 1,ly
      jj = (ly-j)+1
     do i = 1,lx
       ii = (lx-i)+1
      ain(ii,jj) = atmp(i,j)
     end do
    end do
  end subroutine flip2di4
end module ww3grid
