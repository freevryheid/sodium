module mod_crypto_stream
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long
  use mod_crypto_stream_xsalsa20
  implicit none
  private

  public crypto_stream_keybytes
  public crypto_stream_noncebytes
  public crypto_stream_messagebytes_max
  public crypto_stream_primitive
  public crypto_stream
  public crypto_stream_xor
  public crypto_stream_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_KEYBYTES         = &
    SODIUM_crypto_stream_xsalsa20_KEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_NONCEBYTES       = &
    SODIUM_crypto_stream_xsalsa20_NONCEBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_stream_MESSAGEBYTES_MAX = &
    SODIUM_crypto_stream_xsalsa20_MESSAGEBYTES_MAX
  character(len=*), parameter, public :: SODIUM_crypto_stream_PRIMITIVE              = &
    "xsalsa20"

  interface

    function crypto_stream_keybytes() &
    bind(c, name='crypto_stream_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_keybytes

    function crypto_stream_noncebytes() &
    bind(c, name='crypto_stream_noncebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_noncebytes

    function crypto_stream_messagebytes_max() &
    bind(c, name='crypto_stream_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_stream_messagebytes_max

    function bind_crypto_stream_primitive() &
    bind(c, name='crypto_stream_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_stream_primitive

    function crypto_stream(c, clen, n, k) &
    bind(c, name='crypto_stream') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) n, k
    end function crypto_stream

    function crypto_stream_xor(c, m, mlen, n, k) &
    bind(c, name='crypto_stream_xor') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) m
      character(kind=c_char) c
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) n, k
    end function crypto_stream_xor

    subroutine crypto_stream_keygen(k) &
    bind(c, name='crypto_stream_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_stream_keygen

  end interface

contains

  function crypto_stream_primitive() result(res)
    type(c_ptr) res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_stream_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_stream_primitive

end module mod_crypto_stream
