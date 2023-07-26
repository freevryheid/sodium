module mod_crypto_box
  use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_char, c_long_long, c_int, c_int128_t, c_int64_t
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

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_SEEDBYTES          = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_SEEDBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_PUBLICKEYBYTES     = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_PUBLICKEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_SECRETKEYBYTES     = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_SECRETKEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_NONCEBYTES         = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_NONCEBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_MACBYTES           = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_MACBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_MESSAGEBYTES_MAX   = &
    SODIUM_crypto_box_CURVE25519XSALSA20POLY1305_MESSAGEBYTES_MAX
  ! integer(kind=c_int128_t), parameter, public :: SODIUM_crypto_box_MESSAGEBYTES_MAX   = &
    ! SODIUM_crypto_box_CURVE25519XSALSA20POLY1305_MESSAGEBYTES_MAX
  character(len=*), parameter, public :: SODIUM_crypto_box_PRIMITIVE = &
    "curve25519xsalsa20poly1305"
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_BEFORENMBYTES      = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_BEFORENMBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_SEALBYTES          = &
    (SODIUM_crypto_box_PUBLICKEYBYTES + SODIUM_crypto_box_MACBYTES)
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_ZEROBYTES          = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_ZEROBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_box_BOXZEROBYTES       = &
    SODIUM_crypto_box_curve25519xsalsa20poly1305_BOXZEROBYTES

  ! integer, parameter :: int128 = selected_int_kind(19)

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

    ! fortran doesn't have unsigned integers 
    ! - so cannot assign a variable for the returned value 
    function crypto_box_messagebytes_max() &
    bind(c, name='crypto_box_messagebytes_max') &
    result(res)
    ! integer(8) :: res
      import :: c_size_t
      integer(kind=c_size_t) :: res
      ! import :: c_int64_t
      ! integer(kind=c_int64_t) :: res
      ! import :: c_int128_t
      ! integer(kind=c_int128_t) :: res
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

  contains
  
    function crypto_box_primitive() result(res)
      type(c_ptr) :: res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_box_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_box_primitive

    ! function crypto_box_messagebytes_max() result(res)
    !   integer(kind=c_size_t) :: res
    !   res = bind_crypto_box_messagebytes_max()
    !   ! res = res + ishft(1_c_size_t, 32)
    !   ! res = res + 2**32
    ! end function crypto_box_messagebytes_max
  
end module mod_crypto_box