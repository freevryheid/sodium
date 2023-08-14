module mod_crypto_scalarmult
  use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_size_t
  use mod_crypto_scalarmult_curve25519
  use mod_common
  implicit none
  private

  public crypto_scalarmult_bytes
  public crypto_scalarmult_scalarbytes
  public crypto_scalarmult_primitive
  public crypto_scalarmult_base
  public crypto_scalarmult

  integer, parameter, public :: SODIUM_crypto_scalarmult_BYTES              = SODIUM_crypto_scalarmult_curve25519_BYTES
  character(len=*), parameter, public :: SODIUM_crypto_scalarmult_PRIMITIVE = "curve25519"
  integer, parameter, public :: SODIUM_crypto_scalarmult_SCALARBYTES        = SODIUM_crypto_scalarmult_curve25519_SCALARBYTES

  interface

    function crypto_scalarmult_bytes() &
    bind(c, name='crypto_scalarmult_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_scalarmult_bytes

    function crypto_scalarmult_scalarbytes() &
    bind(c, name='crypto_scalarmult_scalarbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_scalarmult_scalarbytes

    function bind_crypto_scalarmult_primitive() &
    bind(c, name='crypto_scalarmult_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_scalarmult_primitive

    function crypto_scalarmult_base(q, n) &
    bind(c, name='crypto_scalarmult_base') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n
    end function crypto_scalarmult_base

    function crypto_scalarmult(q, n, p) &
    bind(c, name='crypto_scalarmult') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n, p
    end function crypto_scalarmult

  end interface

  contains

    function crypto_scalarmult_primitive() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_scalarmult_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_scalarmult_primitive

end module mod_crypto_scalarmult
