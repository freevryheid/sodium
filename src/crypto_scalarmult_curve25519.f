module mod_crypto_scalarmult_curve25519
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int
  use :: mod_common
  implicit none
  private

  public :: crypto_scalarmult_curve25519_bytes
  public :: crypto_scalarmult_curve25519_scalarbytes
  public :: crypto_scalarmult_curve25519
  public :: crypto_scalarmult_curve25519_base

  integer, parameter, public :: PARAM_crypto_scalarmult_curve25519_BYTES       = 32
  integer, parameter, public :: PARAM_crypto_scalarmult_curve25519_SCALARBYTES = 32

  interface

    function crypto_scalarmult_curve25519_bytes() &
    bind(c, name='crypto_scalarmult_curve25519_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_scalarmult_curve25519_bytes

    function crypto_scalarmult_curve25519_scalarbytes() &
    bind(c, name='crypto_scalarmult_curve25519_scalarbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_scalarmult_curve25519_scalarbytes

    function crypto_scalarmult_curve25519(q, n, p) &
    bind(c, name='crypto_scalarmult_curve25519') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: q
      character(kind=c_char) :: n, p
    end function crypto_scalarmult_curve25519

    function crypto_scalarmult_curve25519_base(q, n) &
    bind(c, name='crypto_scalarmult_curve25519_base') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: q
      character(kind=c_char) :: n
    end function crypto_scalarmult_curve25519_base

  end interface

end module mod_crypto_scalarmult_curve25519
