module mod_core
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t
  implicit none
  private

  public sodium_init

  integer(kind=c_size_t), parameter, public :: SODIUM_SIZE_MAX = int(z"FFFFFFFFFFFFFFFF")

  interface

    function sodium_init() bind(c, name='sodium_init') result(res)
      import c_int
      integer(kind=c_int) res
    end function sodium_init

  end interface

end module mod_core
