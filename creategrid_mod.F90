
  allocate(connectionList(2))
    ! bipolar boundary condition at top row: nj
  call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1,     &
        tileIndexB=1, positionVector=(/ni+1, 2*nj+1/),                  &
       orientationVector=(/-1, -2/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! periodic boundary condition along first dimension
  call ESMF_DistGridConnectionSet(connectionList(2), tileIndexA=1,     &
       tileIndexB=1, positionVector=(/ni, 0/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/ni,nj/), &
       connectionList=connectionList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
