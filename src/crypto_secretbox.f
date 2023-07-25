module mod_crypto_secretbox
  use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_char, c_int, c_long_long, c_int128_t
  use :: mod_crypto_secretbox_xsalsa20poly1305
  use :: mod_common
  implicit none
  private

  public :: crypto_secretbox_keybytes
  public :: crypto_secretbox_noncebytes
  public :: crypto_secretbox_macbytes
  public :: crypto_secretbox_primitive
  public :: crypto_secretbox_messagebytes_max
  public :: crypto_secretbox_easy
  public :: crypto_secretbox_open_easy
  public :: crypto_secretbox_detached
  public :: crypto_secretbox_open_detached
  public :: crypto_secretbox_keygen
  public :: crypto_secretbox_zerobytes
  public :: crypto_secretbox_boxzerobytes
  public :: crypto_secretbox
  public :: crypto_secretbox_open

  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_KEYBYTES           = &
    PARAM_crypto_secretbox_xsalsa20poly1305_KEYBYTES
  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_NONCEBYTES         = &
    PARAM_crypto_secretbox_xsalsa20poly1305_NONCEBYTES
  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_MACBYTES           = &
    PARAM_crypto_secretbox_xsalsa20poly1305_MACBYTES
  character(len=*), parameter, public :: PARAM_crypto_secretbox_PRIMITIVE                = &
    "xsalsa20poly1305"
  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_MESSAGEBYTES_MAX = &
    PARAM_crypto_secretbox_xsalsa20poly1305_MESSAGEBYTES_MAX
  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_ZEROBYTES          = &
    PARAM_crypto_secretbox_xsalsa20poly1305_ZEROBYTES
  integer(kind=c_size_t), parameter, public :: PARAM_crypto_secretbox_BOXZEROBYTES       = &
    PARAM_crypto_secretbox_xsalsa20poly1305_BOXZEROBYTES

  interface

    function crypto_secretbox_keybytes() &
    bind(c, name='crypto_secretbox_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_keybytes

    function crypto_secretbox_noncebytes() &
    bind(c, name='crypto_secretbox_noncebytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_noncebytes

    function crypto_secretbox_macbytes() &
    bind(c, name='crypto_secretbox_macbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_macbytes

    function bind_crypto_secretbox_primitive() &
    bind(c, name='crypto_secretbox_primitive') &
    result(res)
      import :: c_ptr
      type(c_ptr) :: res
    end function bind_crypto_secretbox_primitive

    function crypto_secretbox_messagebytes_max() &
    bind(c, name='crypto_secretbox_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_messagebytes_max

    function crypto_secretbox_easy(c, m, mlen, n, k) &
    bind(c, name='crypto_secretbox_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_easy

    function crypto_secretbox_open_easy(m, c, clen, n, k) &
    bind(c, name='crypto_secretbox_open_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_open_easy

    function crypto_secretbox_detached(c, mac, m, mlen, n, k) &
    bind(c, name='crypto_secretbox_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: mac, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_detached

    function crypto_secretbox_open_detached(m, c, mac, clen, n, k) &
    bind(c, name='crypto_secretbox_open_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_open_detached

    subroutine crypto_secretbox_keygen(k) &
    bind(c, name='crypto_secretbox_keygen')
      import :: c_char
      character(kind=c_char) :: k
    end subroutine crypto_secretbox_keygen

    function crypto_secretbox_zerobytes() &
    bind(c, name='crypto_secretbox_zerobytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_zerobytes

    function crypto_secretbox_boxzerobytes() &
    bind(c, name='crypto_secretbox_boxzerobytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_secretbox_boxzerobytes

    function crypto_secretbox(c, m, mlen, n, k) &
    bind(c, name='crypto_secretbox') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_secretbox

    function crypto_secretbox_open(m, c, clen, n, k) &
    bind(c, name='crypto_secretbox_open') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_secretbox_open

  end interface

contains

  function crypto_secretbox_primitive() &
   &result(res)
    type(c_ptr) :: res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_secretbox_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_secretbox_primitive

end module mod_crypto_secretbox
