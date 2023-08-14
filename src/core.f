module mod_core
  use, intrinsic::iso_c_binding, only : c_int, c_size_t, c_int128_t
  implicit none
  private

  public sodium_init

  ! integer, parameter :: int128 = selected_int_kind(19)
  ! integer(kind=int128), parameter, public :: SODIUM_SIZE_MAX = 2*huge(0_c_size_t)+1
  ! integer(kind=c_int128_t), parameter, public :: SODIUM_SIZE_MAX = 2*huge(0_c_size_t)+1
  ! integer(kind=c_int128_t), parameter, public :: SODIUM_SIZE_MAX = 18446744073709551615
 ! integer(kind=c_size_t), parameter, public :: SODIUM_SIZE_MAX = huge(0_c_size_t) ! TODO: let's see if this works
  integer(kind=c_size_t), parameter, public :: SODIUM_SIZE_MAX = int(z"FFFFFFFFFFFFFFFF")

  interface

    function sodium_init() bind(c, name='sodium_init') result(res)
      import c_int
      integer(kind=c_int) res
    end function sodium_init

  end interface

end module mod_core