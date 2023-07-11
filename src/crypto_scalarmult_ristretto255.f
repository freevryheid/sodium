module mod_crypto_scalarmult_ristretto255
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int
  implicit none
  private

  public :: crypto_scalarmult_ristretto255_bytes
  public :: crypto_scalarmult_ristretto255_scalarbytes
  public :: crypto_scalarmult_ristretto255
  public :: crypto_scalarmult_ristretto255_base

  integer, parameter, public :: PARAM_crypto_scalarmult_ristretto255_BYTES       = 32
  integer, parameter, public :: PARAM_crypto_scalarmult_ristretto255_SCALARBYTES = 32

  interface

    function crypto_scalarmult_ristretto255_bytes() &
    bind(c, name='crypto_scalarmult_ristretto255_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_scalarmult_ristretto255_bytes

    function crypto_scalarmult_ristretto255_scalarbytes() &
    bind(c, name='crypto_scalarmult_ristretto255_scalarbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_scalarmult_ristretto255_scalarbytes

    function crypto_scalarmult_ristretto255(q, n, p) &
    bind(c, name='crypto_scalarmult_ristretto255') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: q
      character(kind=c_char) :: n, p
    end function crypto_scalarmult_ristretto255

    function crypto_scalarmult_ristretto255_base(q, n) &
    bind(c, name='crypto_scalarmult_ristretto255_base') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: q
      character(kind=c_char) :: n
    end function crypto_scalarmult_ristretto255_base

  end interface

end module mod_crypto_scalarmult_ristretto255