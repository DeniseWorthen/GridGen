module physcon

  implicit none

  integer,parameter :: R8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: R4 = selected_real_kind( 6) ! 4 byte real
  integer,parameter :: I4 = selected_int_kind ( 6) ! 4 byte integer

  real(R8), parameter :: pi = 3.14159265358979323846_R8
  real(R8), parameter :: deg2rad = pi/180.0_R8

end module physcon
