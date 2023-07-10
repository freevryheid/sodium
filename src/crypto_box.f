module mod_crypto_box

  use, intrinsic :: iso_c_binding
  use :: mod_crypto_box_curve25519xsalsa20poly1305
  use :: mod_common

  implicit none

  private

  public :: crypto_box_seedbytes
  public :: crypto_box_publickeybytes
  public :: crypto_box_secretkeybytes
  public :: crypto_box_noncebytes
  public :: crypto_box_macbytes
  public :: crypto_box_messagebytes_max
  public :: crypto_box_primitive
  public :: crypto_box_seed_keypair
  public :: crypto_box_keypair
  public :: crypto_box_easy
  public :: crypto_box_open_easy
  public :: crypto_box_detached
  public :: crypto_box_open_detached
  public :: crypto_box_beforenmbytes
  public :: crypto_box_beforenm
  public :: crypto_box_easy_afternm
  public :: crypto_box_open_easy_afternm
  public :: crypto_box_detached_afternm
  public :: crypto_box_open_detached_afternm
  public :: crypto_box_sealbytes
  public :: crypto_box_seal
  public :: crypto_box_seal_open
  public :: crypto_box_zerobytes
  public :: crypto_box_boxzerobytes
  public :: crypto_box
  public :: crypto_box_open
  public :: crypto_box_afternm
  public :: crypto_box_open_afternm

  integer, parameter, public :: PARAM_CRYPTO_BOX_SEEDBYTES        = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SEEDBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_PUBLICKEYBYTES   = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_PUBLICKEYBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_SECRETKEYBYTES   = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_SECRETKEYBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_NONCEBYTES       = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_NONCEBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_MACBYTES         = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_MACBYTES
  ! integer, parameter, public :: PARAM_CRYPTO_BOX_MESSAGEBYTES_MAX = &
  !   PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_MESSAGEBYTES_MAX
  ! integer, parameter, public :: PARAM_CRYPTO_BOX_PRIMITIVE        = &
    ! PARAM_"curve25519xsalsa20poly1305"
  integer, parameter, public :: PARAM_CRYPTO_BOX_BEFORENMBYTES    = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_BEFORENMBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_SEALBYTES        = &
    PARAM_(CRYPTO_BOX_PUBLICKEYBYTES + CRYPTO_BOX_MACBYTES)
  integer, parameter, public :: PARAM_CRYPTO_BOX_ZEROBYTES        = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_ZEROBYTES
  integer, parameter, public :: PARAM_CRYPTO_BOX_BOXZEROBYTES     = &
    PARAM_CRYPTO_BOX_CURVE25519XSALSA20POLY1305_BOXZEROBYTES

  interface

    function crypto_box_seedbytes() &
    bind(c, name='crypto_box_seedbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_seedbytes

    function crypto_box_publickeybytes() &
    bind(c, name='crypto_box_publickeybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_publickeybytes

    function crypto_box_secretkeybytes() &
    bind(c, name='crypto_box_secretkeybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_secretkeybytes

    function crypto_box_noncebytes() &
    bind(c, name='crypto_box_noncebytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_noncebytes

    function crypto_box_macbytes() &
    bind(c, name='crypto_box_macbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_macbytes

    function crypto_box_messagebytes_max() &
    bind(c, name='crypto_box_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_messagebytes_max

    function bind_crypto_box_primitive() &
    bind(c, name='crypto_box_primitive') &
    result(res)
      import :: c_ptr
      type(c_ptr) :: res
    end function bind_crypto_box_primitive

    function crypto_box_seed_keypair(pk, sk, seed) &
    bind(c, name='crypto_box_seed_keypair') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: pk, sk
      character(kind=c_char) :: seed
    end function crypto_box_seed_keypair

    function crypto_box_keypair(pk, sk) &
    bind(c, name='crypto_box_keypair') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: pk, sk
    end function crypto_box_keypair

    function crypto_box_easy(c, m, mlen, n, pk, sk) &
    bind(c, name='crypto_box_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_easy

    function crypto_box_open_easy(m, c, clen, n, pk, sk) &
    bind(c, name='crypto_box_open_easy') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_open_easy

    function crypto_box_detached(c, mac, m, mlen, n, pk, sk) &
    bind(c, name='crypto_box_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: mac, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_detached

    function crypto_box_open_detached(m, c, mac, clen, n, pk, sk) &
    bind(c, name='crypto_box_open_detached') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_open_detached

    function crypto_box_beforenmbytes() &
    bind(c, name='crypto_box_beforenmbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_beforenmbytes

    function crypto_box_beforenm(k, pk, sk) &
    bind(c, name='crypto_box_beforenm') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: k
      character(kind=c_char) :: pk, sk
    end function crypto_box_beforenm

    function crypto_box_easy_afternm(c, m, mlen, n, k) &
    bind(c, name='crypto_box_easy_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_box_easy_afternm

    function crypto_box_open_easy_afternm(m, c, clen, n, k) &
    bind(c, name='crypto_box_open_easy_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_box_open_easy_afternm

    function crypto_box_detached_afternm(c, mac, m, mlen, n, k) &
    bind(c, name='crypto_box_detached_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: mac, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_box_detached_afternm

    function crypto_box_open_detached_afternm(m, c, mac, clen, n, k) &
    bind(c, name='crypto_box_open_detached_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_box_open_detached_afternm

    function crypto_box_sealbytes() &
    bind(c, name='crypto_box_sealbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_sealbytes

    function crypto_box_seal(c, m, mlen, pk) &
    bind(c, name='crypto_box_seal') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: pk
    end function crypto_box_seal

    function crypto_box_seal_open(m, c, clen, pk, sk) &
    bind(c, name='crypto_box_seal_open') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: pk, sk
    end function crypto_box_seal_open

    function crypto_box_zerobytes() &
    bind(c, name='crypto_box_zerobytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_zerobytes

    function crypto_box_boxzerobytes() &
    bind(c, name='crypto_box_boxzerobytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_boxzerobytes

    function crypto_box(c, m, mlen, n, pk, sk) &
    bind(c, name='crypto_box') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box

    function crypto_box_open(m, c, clen, n, pk, sk) &
    bind(c, name='crypto_box_open') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_open

    function crypto_box_afternm(c, m, mlen, n, k) &
    bind(c, name='crypto_box_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_box_afternm

    function crypto_box_open_afternm(m, c, clen, n, k) &
    bind(c, name='crypto_box_open_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_box_open_afternm

  end interface

  ! contains

  !   function crypto_box_seedbytes() result(res)
  !     integer res
  !     res = int(bind_crypto_box_seedbytes())
  !   end function crypto_box_seedbytes

  !   function crypto_box_publickeybytes() result(res)
  !     integer res
  !     res = int(bind_crypto_box_publickeybytes())
  !   end function crypto_box_publickeybytes

  !   function crypto_box_secretkeybytes() result(res)
  !     integer res
  !     res = int(bind_crypto_box_secretkeybytes())
  !   end function crypto_box_secretkeybytes

  !   function crypto_box_noncebytes() result(res)
  !     integer res
  !     res = int(bind_crypto_box_noncebytes())
  !   end function crypto_box_noncebytes

  !   function crypto_box_macbytes() result(res)
  !     integer res
  !     res = int(bind_crypto_box_macbytes())
  !   end function crypto_box_macbytes

  !   function crypto_box_messagebytes_max() result(res)
  !     integer res
  !     res = int(bind_crypto_box_messagebytes_max())
  !   end function crypto_box_messagebytes_max

  !   function crypto_box_primitive() result(res)
  !     type(c_ptr) :: res1
  !     character(len=:), allocatable :: res
  !     res1 = bind_crypto_box_primitive()
  !     call c_f_str_ptr(res1, res)
  !   end function crypto_box_primitive

end module mod_crypto_box
