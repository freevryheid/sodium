module mod_crypto_kx
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char, c_ptr
  use mod_common
  implicit none
  private

  public crypto_kx_publickeybytes
  public crypto_kx_secretkeybytes
  public crypto_kx_seedbytes
  public crypto_kx_sessionkeybytes
  public crypto_kx_primitive
  public crypto_kx_seed_keypair
  public crypto_kx_keypair
  public crypto_kx_client_session_keys
  public crypto_kx_server_session_keys

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_kx_PUBLICKEYBYTES  = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_kx_SECRETKEYBYTES  = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_kx_SEEDBYTES       = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_kx_SESSIONKEYBYTES = 32
  character(len=*), parameter, public :: SODIUM_crypto_kx_PRIMITIVE             = "x25519blake2b"

  interface

    function crypto_kx_publickeybytes() &
    bind(c, name='crypto_kx_publickeybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kx_publickeybytes

    function crypto_kx_secretkeybytes() &
    bind(c, name='crypto_kx_secretkeybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kx_secretkeybytes

    function crypto_kx_seedbytes() &
    bind(c, name='crypto_kx_seedbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kx_seedbytes

    function crypto_kx_sessionkeybytes() &
    bind(c, name='crypto_kx_sessionkeybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kx_sessionkeybytes

    function bind_crypto_kx_primitive() &
    bind(c, name='crypto_kx_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_kx_primitive

    function crypto_kx_seed_keypair(pk, sk, seed) &
    bind(c, name='crypto_kx_seed_keypair') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) pk, sk
      character(kind=c_char) seed
    end function crypto_kx_seed_keypair

    function crypto_kx_keypair(pk, sk) &
    bind(c, name='crypto_kx_keypair') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) pk, sk
    end function crypto_kx_keypair

    ! TODO: testme
    function crypto_kx_client_session_keys(rx, tx, client_pk, client_sk, server_pk) &
    bind(c, name='crypto_kx_client_session_keys') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) rx, tx
      character(kind=c_char) client_pk, client_sk, server_pk
    end function crypto_kx_client_session_keys

    ! TODO: testme
    function crypto_kx_server_session_keys(rx, tx, server_pk, server_sk, client_pk) &
    bind(c, name='crypto_kx_server_session_keys') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) rx, tx
      character(kind=c_char) client_pk, server_pk, server_sk
    end function crypto_kx_server_session_keys

  end interface

contains

  function crypto_kx_primitive() result(res)
    type(c_ptr) res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_kx_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_kx_primitive

end module mod_crypto_kx
