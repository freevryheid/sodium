module mod_crypto_stream_salsa2012
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long
  implicit none
  private

  public crypto_stream_salsa2012_keybytes
  public crypto_stream_salsa2012_noncebytes
  public crypto_stream_salsa2012_messagebytes_max
  public crypto_stream_salsa2012
  public crypto_stream_salsa2012_xor
  public crypto_stream_salsa2012_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_salsa2012_KEYBYTES         = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_salsa2012_NONCEBYTES       = 8
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_salsa2012_MESSAGEBYTES_MAX = SODIUM_SIZE_MAX

  interface

    function crypto_stream_salsa2012_keybytes() &
    bind(c, name='crypto_stream_salsa2012_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_salsa2012_keybytes

    function crypto_stream_salsa2012_noncebytes() &
    bind(c, name='crypto_stream_salsa2012_noncebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_salsa2012_noncebytes

    function crypto_stream_salsa2012_messagebytes_max() &
    bind(c, name='crypto_stream_salsa2012_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_salsa2012_messagebytes_max

    function crypto_stream_salsa2012(c, clen, n, k) &
    bind(c, name='crypto_stream_salsa2012') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) n, k
    end function crypto_stream_salsa2012

    function crypto_stream_salsa2012_xor(c, m, mlen, n, k) &
    bind(c, name='crypto_stream_salsa2012_xor') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) m
      character(kind=c_char) c
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) n, k
    end function crypto_stream_salsa2012_xor

    subroutine crypto_stream_salsa2012_keygen(k) &
    bind(c, name='crypto_stream_salsa2012_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_stream_salsa2012_keygen

  end interface

end module mod_crypto_stream_salsa2012
