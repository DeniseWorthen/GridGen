!> @file
!! @brief Create the ESMF weights for post
!! @author Denise.Worthen@noaa.gov
!!
!> This module creates the ESMF weights used to remap from the tripole grid to a rectilinear grid
!! @author Denise.Worthen@noaa.gov
module postwgts

  use ESMF

  use gengrid_kinds, only : XL,CL,CM,CS
  use grdvars,       only : nv
  use charstrings,   only : dirout, res, staggerlocs, logmsg
  use netcdf

  implicit none

contains
  !> Create the ESMF weights files to remap velocity points from their native stagger location to the center
  !! (Ct) location. Create the ESMF weights file to remap from the Ct location to a rectilinear grid
  !!
  !! @author Denise.Worthen@noaa.gov

  subroutine make_postwgts

    ! local variables
    character(len=CL) :: fsrc, fdst, fwgt
    character(len=CL) :: cmdstr
    character(len= 2) :: cstagger

    character(len=CM), dimension(2) :: methodname = (/'conserve', 'bilinear'/)

    type(ESMF_RegridMethod_Flag) :: method
    type(ESMF_RouteHandle)       :: RH
    ! the number of possible destination grids depends on the source grid resolution
    integer :: system
    integer :: k,rc,nd,ndest
    character(len=CS), allocatable, dimension(:) :: destgrds

    !---------------------------------------------------------------------
    ! set the destination grids
    !---------------------------------------------------------------------

    if(trim(res) .eq. '400')return

    if(trim(res) .eq. '100')then
       ndest = 1
       allocate(destgrds(ndest))
       destgrds = (/'1p0 '/)
    end if
    if(trim(res) .eq. '050')then
       ndest = 2
       allocate(destgrds(ndest))
       destgrds = (/'1p0 ', '0p5 '/)
    end if
    if(trim(res) .eq. '025')then
       ndest = 3
       allocate(destgrds(ndest))
       destgrds = (/'1p0 ', '0p5 ', '0p25'/)
    end if

    !---------------------------------------------------------------------
    ! use ESMF to create the weights for unstaggering the points onto
    ! the Ct staggers for post; the destination is always Ct
    !---------------------------------------------------------------------
#ifdef test
    method=ESMF_REGRIDMETHOD_BILINEAR
    fdst = trim(dirout)//'/'//'Ct.mx'//trim(res)//'_SCRIP.nc'
    do k = 2,nv
       cstagger = trim(staggerlocs(k))
       fsrc = trim(dirout)//'/'//trim(cstagger)//'.mx'//trim(res)//'_SCRIP.nc'
       fwgt = trim(dirout)//'/'//'tripole.mx'//trim(res)//'.'//trim(cstagger)//'.to.Ct.bilinear.nc'
       logmsg = 'creating weight file '//trim(fwgt)
       print '(a)',trim(logmsg)

       call ESMF_RegridWeightGen(srcFile=trim(fsrc),dstFile=trim(fdst), &
            weightFile=trim(fwgt), regridmethod=method, &
            ignoreDegenerate=.true., &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do
#endif
    !---------------------------------------------------------------------
    ! use ESMF to create the weights from the Ct tripole to the rectilinear
    ! grids with conservative and bilinear methods for post; the source
    ! file is always Ct
    !---------------------------------------------------------------------

    ! generate ESMF mesh from source SCRIP file
    fsrc = trim(dirout)//'/'//'Ct.mx'//trim(res)//'_SCRIP.nc'
    !fdst = trim(dirout)//'/'//'Ct.mx'//trim(res)//'_mesh.nc'
    !cmdstr = 'srun -A nems --nodes=1 ESMF_Scrip2Unstruct '//trim(fsrc)//' '//trim(fdst)//' 0'
    !cmdstr = 'srun ESMF_Scrip2Unstruct '//trim(fsrc)//' '//trim(fdst)//' 0'
    !print '(A)',trim(cmdstr)
    !call execute_command_line(trim(cmdstr), wait=.true.)
    !rc = system(trim(cmdstr))
    !print *,'here'
!#ifdef test
!    fsrc = trim(dirout)//'/'//'Ct.mx'//trim(res)//'_mesh.nc'
    do nd = 1,ndest
       fdst = trim(dirout)//'/rect.'//trim(destgrds(nd))//'_SCRIP.nc'

       do k = 1,size(methodname)
          if(trim(methodname(k)) .eq. 'bilinear')method=ESMF_REGRIDMETHOD_BILINEAR
          if(trim(methodname(k)) .eq. 'conserve')method=ESMF_REGRIDMETHOD_CONSERVE

          fwgt = trim(dirout)//'/'//'tripole.mx'//trim(res)//'.Ct.to.rect.'//trim(destgrds(nd)) &
               //'.'//trim(methodname(k))//'.nc'
          logmsg = 'creating weight file '//trim(fwgt)
          print '(a)',trim(logmsg)

          call ESMF_RegridWeightGen(srcFile=trim(fsrc),dstFile=trim(fdst),       &
               regridRouteHandle=RH, weightFile=trim(fwgt), regridmethod=method, &
               ignoreDegenerate=.true.,                                          &
               unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
       end do
    end do
!#endif
    deallocate(destgrds)

  end subroutine make_postwgts
end module postwgts
