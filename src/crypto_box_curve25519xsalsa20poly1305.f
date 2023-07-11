module mod_crypto_box_curve25519xsalsa20poly1305
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_long_long, c_int
  implicit none
  private

  public :: crypto_box_curve25519xsalsa20poly1305_seedbytes
  public :: crypto_box_curve25519xsalsa20poly1305_publickeybytes
  public :: crypto_box_curve25519xsalsa20poly1305_secretkeybytes
  public :: crypto_box_curve25519xsalsa20poly1305_beforenmbytes
  public :: crypto_box_curve25519xsalsa20poly1305_noncebytes
  public :: crypto_box_curve25519xsalsa20poly1305_macbytes
  public :: crypto_box_curve25519xsalsa20poly1305_messagebytes_max
  public :: crypto_box_curve25519xsalsa20poly1305_seed_keypair
  public :: crypto_box_curve25519xsalsa20poly1305_keypair
  public :: crypto_box_curve25519xsalsa20poly1305_beforenm
  public :: crypto_box_curve25519xsalsa20poly1305_boxzerobytes
  public :: crypto_box_curve25519xsalsa20poly1305_zerobytes
  public :: crypto_box_curve25519xsalsa20poly1305
  public :: crypto_box_curve25519xsalsa20poly1305_open
  public :: crypto_box_curve25519xsalsa20poly1305_afternm
  public :: crypto_box_curve25519xsalsa20poly1305_open_afternm

  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_SEEDBYTES        = 32
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_PUBLICKEYBYTES   = 32
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_SECRETKEYBYTES   = 32
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_BEFORENMBYTES    = 32
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_NONCEBYTES       = 24
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_MACBYTES         = 16
  integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_BOXZEROBYTES     = 16
  ! integer, parameter, public :: PARAM_crypto_box_curve25519xsalsa20poly1305_ZEROBYTES        = &
  !   (crypto_box_curve25519xsalsa20poly1305_BOXZEROBYTES + crypto_box_curve25519xsalsa20poly1305_MACBYTES)       
   
  interface

    function crypto_box_curve25519xsalsa20poly1305_seedbytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_seedbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_seedbytes

    function crypto_box_curve25519xsalsa20poly1305_publickeybytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_publickeybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_publickeybytes

    function crypto_box_curve25519xsalsa20poly1305_secretkeybytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_secretkeybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_secretkeybytes

    function crypto_box_curve25519xsalsa20poly1305_beforenmbytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_beforenmbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_beforenmbytes

    function crypto_box_curve25519xsalsa20poly1305_noncebytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_noncebytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_noncebytes

    function crypto_box_curve25519xsalsa20poly1305_macbytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_macbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_macbytes

    function crypto_box_curve25519xsalsa20poly1305_messagebytes_max() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_messagebytes_max

    function crypto_box_curve25519xsalsa20poly1305_seed_keypair(pk, sk, seed) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_seed_keypair') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: pk, sk
      character(kind=c_char) :: seed
    end function crypto_box_curve25519xsalsa20poly1305_seed_keypair

    function crypto_box_curve25519xsalsa20poly1305_keypair(pk, sk) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_keypair') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: pk, sk
    end function crypto_box_curve25519xsalsa20poly1305_keypair

    function crypto_box_curve25519xsalsa20poly1305_beforenm(k, pk, sk) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_beforenm') &
    result(res)
      import :: c_char, c_int
      integer(kind=c_int) :: res
      character(kind=c_char) :: k
      character(kind=c_char) :: pk, sk
    end function crypto_box_curve25519xsalsa20poly1305_beforenm

    function crypto_box_curve25519xsalsa20poly1305_boxzerobytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_boxzerobytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_boxzerobytes

    function crypto_box_curve25519xsalsa20poly1305_zerobytes() &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_zerobytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_box_curve25519xsalsa20poly1305_zerobytes

    function crypto_box_curve25519xsalsa20poly1305(c, m, mlen, n, pk, sk) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_curve25519xsalsa20poly1305

    function crypto_box_curve25519xsalsa20poly1305_open(m, c, clen, n, pk, sk) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_open') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, pk, sk
    end function crypto_box_curve25519xsalsa20poly1305_open

    function crypto_box_curve25519xsalsa20poly1305_afternm(c, m, mlen, n, k) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      character(kind=c_char) :: m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) :: n, k
    end function crypto_box_curve25519xsalsa20poly1305_afternm

    function crypto_box_curve25519xsalsa20poly1305_open_afternm(m, c, clen, n, k) &
    bind(c, name='crypto_box_curve25519xsalsa20poly1305_open_afternm') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: m
      character(kind=c_char) :: c
      integer(kind=c_long_long), value :: clen
      character(kind=c_char) :: n, k
    end function crypto_box_curve25519xsalsa20poly1305_open_afternm

  end interface

end module mod_crypto_box_curve25519xsalsa20poly1305
