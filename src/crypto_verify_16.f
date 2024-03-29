module mod_crypto_verify_16
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int
  implicit none
  private

  public crypto_verify_16_bytes
  public crypto_verify_16

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_verify_16_BYTES = 16

  interface

    function crypto_verify_16_bytes() &
    bind(c, name='crypto_verify_16_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_verify_16_bytes

    function crypto_verify_16(x, y) &
    bind(c, name='crypto_verify_16') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) x, y
    end function crypto_verify_16

  end interface

end module mod_crypto_verify_16
