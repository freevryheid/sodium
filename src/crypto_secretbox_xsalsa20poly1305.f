module mod_crypto_secretbox_xsalsa20poly1305
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_int128_t
  use mod_crypto_stream_xsalsa20
  implicit none
  private

  public crypto_secretbox_xsalsa20poly1305_keybytes
  public crypto_secretbox_xsalsa20poly1305_noncebytes
  public crypto_secretbox_xsalsa20poly1305_macbytes
  public crypto_secretbox_xsalsa20poly1305_messagebytes_max
  public crypto_secretbox_xsalsa20poly1305
  public crypto_secretbox_xsalsa20poly1305_open
  public crypto_secretbox_xsalsa20poly1305_keygen
  public crypto_secretbox_xsalsa20poly1305_boxzerobytes
  public crypto_secretbox_xsalsa20poly1305_zerobytes

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_KEYBYTES         = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_NONCEBYTES       = 24
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_MACBYTES         = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_MESSAGEBYTES_MAX = &
    (SODIUM_crypto_stream_xsalsa20_MESSAGEBYTES_MAX - SODIUM_crypto_secretbox_xsalsa20poly1305_MACBYTES)
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_BOXZEROBYTES     = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretbox_xsalsa20poly1305_ZEROBYTES        = &
    (SODIUM_crypto_secretbox_xsalsa20poly1305_BOXZEROBYTES + SODIUM_crypto_secretbox_xsalsa20poly1305_MACBYTES)

  interface

    function crypto_secretbox_xsalsa20poly1305_keybytes() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_keybytes

    function crypto_secretbox_xsalsa20poly1305_noncebytes() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_noncebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_noncebytes

    function crypto_secretbox_xsalsa20poly1305_macbytes() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_macbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_macbytes

    function crypto_secretbox_xsalsa20poly1305_messagebytes_max() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_messagebytes_max

    function bind_crypto_secretbox_xsalsa20poly1305(c, m, mlen, n, k) &
    bind(c, name='crypto_secretbox_xsalsa20poly1305') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      character(kind=c_char) m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) n, k
    end function bind_crypto_secretbox_xsalsa20poly1305

    function bind_crypto_secretbox_xsalsa20poly1305_open(m, c, clen, n, k) &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_open') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) m
      character(kind=c_char) c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) n, k
    end function bind_crypto_secretbox_xsalsa20poly1305_open

    subroutine crypto_secretbox_xsalsa20poly1305_keygen(k) &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_secretbox_xsalsa20poly1305_keygen

    function crypto_secretbox_xsalsa20poly1305_boxzerobytes() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_boxzerobytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_boxzerobytes

    function crypto_secretbox_xsalsa20poly1305_zerobytes() &
    bind(c, name='crypto_secretbox_xsalsa20poly1305_zerobytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretbox_xsalsa20poly1305_zerobytes

  end interface

  contains

    function crypto_secretbox_xsalsa20poly1305(c, m, n, k) result(res)
      integer(kind=c_int) res
      character(len=*) c
      character(len=*) m
      integer(kind=c_long_long) mlen
      character(len=*) n, k
      mlen = len(m)
      res = bind_crypto_secretbox_xsalsa20poly1305(c, m, mlen, n, k)
    end function crypto_secretbox_xsalsa20poly1305

    function crypto_secretbox_xsalsa20poly1305_open(m, c, n, k) result(res)
      integer(kind=c_int) res
      character(len=*) m
      character(len=*) c
      integer(kind=c_long_long) clen
      character(len=*) n, k
      clen = len(c)
      res = bind_crypto_secretbox_xsalsa20poly1305_open(m, c, clen, n, k)
    end function crypto_secretbox_xsalsa20poly1305_open

end module mod_crypto_secretbox_xsalsa20poly1305
