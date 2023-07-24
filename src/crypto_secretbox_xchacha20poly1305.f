module mod_crypto_secretbox_xchacha20poly1305
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long
  implicit none
  private

  public :: crypto_secretbox_xchacha20poly1305_keybytes
  public :: crypto_secretbox_xchacha20poly1305_noncebytes
  public :: crypto_secretbox_xchacha20poly1305_macbytes
  public :: crypto_secretbox_xchacha20poly1305_messagebytes_max
  public :: crypto_secretbox_xchacha20poly1305_easy
  public :: crypto_secretbox_xchacha20poly1305_open_easy
  public :: crypto_secretbox_xchacha20poly1305_detached
  public :: crypto_secretbox_xchacha20poly1305_open_detached

  integer, parameter, public :: PARAM_crypto_secretbox_xchacha20poly1305_KEYBYTES   = 32
  integer, parameter, public :: PARAM_crypto_secretbox_xchacha20poly1305_NONCEBYTES = 24
  integer, parameter, public :: PARAM_crypto_secretbox_xchacha20poly1305_MACBYTES   = 16

  interface

    function crypto_secretbox_xchacha20poly1305_keybytes() &
    bind(c, name='crypto_secretbox_xchacha20poly1305_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_xchacha20poly1305_keybytes

    function crypto_secretbox_xchacha20poly1305_noncebytes() &
    bind(c, name='crypto_secretbox_xchacha20poly1305_noncebytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_xchacha20poly1305_noncebytes

    function crypto_secretbox_xchacha20poly1305_macbytes() &
    bind(c, name='crypto_secretbox_xchacha20poly1305_macbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_xchacha20poly1305_macbytes

    function crypto_secretbox_xchacha20poly1305_messagebytes_max() &
    bind(c, name='crypto_secretbox_xchacha20poly1305_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_xchacha20poly1305_messagebytes_max

    function crypto_secretbox_xchacha20poly1305_easy(c, m, mlen, n, k) &
    bind(c, name='crypto_secretbox_xchacha20poly1305_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_xchacha20poly1305_easy

    function crypto_secretbox_xchacha20poly1305_open_easy(m, c, clen, n, k) &
    bind(c, name='crypto_secretbox_xchacha20poly1305_open_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_xchacha20poly1305_open_easy

    function crypto_secretbox_xchacha20poly1305_detached(c, mac, m, mlen, n, k) &
    bind(c, name='crypto_secretbox_xchacha20poly1305_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: mac, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_xchacha20poly1305_detached

    function crypto_secretbox_xchacha20poly1305_open_detached(m, c, mac, clen, n, k) &
    bind(c, name='crypto_secretbox_xchacha20poly1305_open_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_xchacha20poly1305_open_detached

  end interface

end module mod_crypto_secretbox_xchacha20poly1305
