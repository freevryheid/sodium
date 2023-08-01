module mod_crypto_shorthash_siphash24
  use, intrinsic :: iso_c_binding
  use :: mod_common
  implicit none
  private

  public :: crypto_shorthash_siphash24_bytes
  public :: crypto_shorthash_siphash24_keybytes
  public :: crypto_shorthash_siphash24
  public :: crypto_shorthash_siphashx24_bytes
  public :: crypto_shorthash_siphashx24_keybytes
  public :: crypto_shorthash_siphashx24

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_siphash24_BYTES     = 8
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_siphash24_KEYBYTES  = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_siphashx24_BYTES    = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_siphashx24_KEYBYTES = 16

  interface

    function crypto_shorthash_siphash24_bytes() &
    bind(c, name='crypto_shorthash_siphash24_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_shorthash_siphash24_bytes

    function crypto_shorthash_siphash24_keybytes() &
    bind(c, name='crypto_shorthash_siphash24_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_shorthash_siphash24_keybytes

    function crypto_shorthash_siphash24(out, in, inlen, k) &
    bind(c, name='crypto_shorthash_siphash24') &
    result(res)
      import :: c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) :: k
    end function crypto_shorthash_siphash24

    function crypto_shorthash_siphashx24_bytes() &
    bind(c, name='crypto_shorthash_siphashx24_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_shorthash_siphashx24_bytes

    function crypto_shorthash_siphashx24_keybytes() &
    bind(c, name='crypto_shorthash_siphashx24_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_shorthash_siphashx24_keybytes

    function crypto_shorthash_siphashx24(out, in, inlen, k) &
    bind(c, name='crypto_shorthash_siphashx24') &
    result(res)
      import :: c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) :: k
    end function crypto_shorthash_siphashx24

  end interface

end module mod_crypto_shorthash_siphash24
