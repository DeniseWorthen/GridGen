module icegriddefs

  implicit none

  integer, parameter :: ncicevars = 20

  type icedefs
    character(len=12)   ::  var_name
    character(len=64)   :: long_name
    character(len=12)   :: unit_name
    character(len= 2)   ::  var_type
  end type icedefs

  type(icedefs) :: icegrid(ncicevars)
  contains

  subroutine ice_typedefine

  integer :: ii = 0
  
   !default
   icegrid(:)%var_type = 'r8'

   ii = ii + 1
   icegrid(ii)%var_name  = 'ulon'
   icegrid(ii)%long_name = 'Longitude of corner (Bu) points'
   icegrid(ii)%unit_name = 'radians'

   ii = ii + 1
   icegrid(ii)%var_name  = 'ulat'
   icegrid(ii)%long_name = 'Latitude of corner (Bu) points'
   icegrid(ii)%unit_name = 'radians'

   ii = ii + 1
   icegrid(ii)%var_name  = 'hte'
   icegrid(ii)%long_name = 'Distance between corner (Bu) points, east face'
   icegrid(ii)%unit_name = 'cm'

   ii = ii + 1
   icegrid(ii)%var_name  = 'htn'
   icegrid(ii)%long_name = 'Distance between corner (Bu) points, north face'
   icegrid(ii)%unit_name = 'cm'

   ii = ii + 1
   icegrid(ii)%var_name  = 'angle'
   icegrid(ii)%long_name = 'Angle at corner (Bu) points'
   icegrid(ii)%unit_name = 'radians'

   ii = ii + 1
   icegrid(ii)%var_name  = 'kmt'
   icegrid(ii)%long_name = 'ocean fraction at T-cell centers'
   icegrid(ii)%unit_name = 'none'
   icegrid(ii)%var_type  = 'i4'

 end subroutine ice_typedefine
end module icegriddefs
