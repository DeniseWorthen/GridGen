module scripgriddefs

  implicit none

  integer, parameter :: nscripvars = 4

  type grid_defs
    character(len=20)   ::  var_name
    character(len=20)   :: unit_name
    character(len= 2)   ::  var_type
  end type grid_defs

  type(grid_defs) :: scgrid(nscripvars)
  contains

  subroutine scripgrid_typedefine

  integer :: ii = 0
  
   !default
   scgrid(:)%var_type  = 'r8'

   ii = ii + 1
   scgrid(ii)%var_name  = 'grid_center_lat'
   scgrid(ii)%unit_name = 'degrees'

   ii = ii + 1
   scgrid(ii)%var_name  = 'grid_center_lon'
   scgrid(ii)%unit_name = 'degrees'

   ii = ii + 1
   scgrid(ii)%var_name  = 'grid_corner_lat'
   scgrid(ii)%unit_name = 'degrees'

   ii = ii + 1
   scgrid(ii)%var_name  = 'grid_corner_lon'
   scgrid(ii)%unit_name = 'degrees'

 end subroutine scripgrid_typedefine
end module scripgriddefs
