module mod_crypto_scalarmult_ed25519
  use, intrinsic :: iso_c_binding, only : c_int, c_char, c_size_t
  use mod_common
  implicit none
  private

  public crypto_scalarmult_ed25519_bytes
  public crypto_scalarmult_ed25519_scalarbytes
  public crypto_scalarmult_ed25519
  public crypto_scalarmult_ed25519_noclamp
  public crypto_scalarmult_ed25519_base
  public crypto_scalarmult_ed25519_base_noclamp

  integer, parameter, public :: SODIUM_crypto_scalarmult_ed25519_BYTES       = 32
  integer, parameter, public :: SODIUM_crypto_scalarmult_ed25519_SCALARBYTES = 32

  interface

    function crypto_scalarmult_ed25519_bytes() &
    bind(c, name='crypto_scalarmult_ed25519_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_scalarmult_ed25519_bytes

    function crypto_scalarmult_ed25519_scalarbytes() &
    bind(c, name='crypto_scalarmult_ed25519_scalarbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_scalarmult_ed25519_scalarbytes

    function crypto_scalarmult_ed25519(q, n, p) &
    bind(c, name='crypto_scalarmult_ed25519') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n, p
    end function crypto_scalarmult_ed25519

    function crypto_scalarmult_ed25519_noclamp(q, n, p) &
    bind(c, name='crypto_scalarmult_ed25519_noclamp') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n, p
    end function crypto_scalarmult_ed25519_noclamp

    function crypto_scalarmult_ed25519_base(q, n) &
    bind(c, name='crypto_scalarmult_ed25519_base') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n
    end function crypto_scalarmult_ed25519_base

    function crypto_scalarmult_ed25519_base_noclamp(q, n) &
    bind(c, name='crypto_scalarmult_ed25519_base_noclamp') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) q
      character(kind=c_char) n
    end function crypto_scalarmult_ed25519_base_noclamp

  end interface

end module mod_crypto_scalarmult_ed25519
