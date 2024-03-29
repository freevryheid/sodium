module mod_crypto_sign
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_long_long, c_char, c_ptr
  use mod_common
  use mod_crypto_hash_sha512, only: crypto_hash_sha512_state
  use mod_crypto_sign_ed25519
  implicit none
  private

  public crypto_sign_statebytes
  public crypto_sign_bytes
  public crypto_sign_seedbytes
  public crypto_sign_publickeybytes
  public crypto_sign_secretkeybytes
  public crypto_sign_messagebytes_max
  public crypto_sign_primitive
  public crypto_sign_seed_keypair
  public crypto_sign_keypair
  public crypto_sign
  public crypto_sign_open
  public crypto_sign_detached
  public crypto_sign_verify_detached
  public crypto_sign_init
  public crypto_sign_update
  public crypto_sign_final_create
  public crypto_sign_final_verify

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_sign_BYTES            = SODIUM_crypto_sign_ed25519_BYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_sign_SEEDBYTES        = SODIUM_crypto_sign_ed25519_SEEDBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_sign_PUBLICKEYBYTES   = SODIUM_crypto_sign_ed25519_PUBLICKEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_sign_SECRETKEYBYTES   = SODIUM_crypto_sign_ed25519_SECRETKEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_sign_MESSAGEBYTES_MAX = SODIUM_crypto_sign_ed25519_MESSAGEBYTES_MAX
  character(len=*), parameter, public :: SODIUM_crypto_sign_PRIMITIVE              = "ed25519"

  type, public, bind(c) :: crypto_sign_state
    type(crypto_hash_sha512_state) hs
  end type

  interface

    function crypto_sign_statebytes() &
    bind(c, name='crypto_sign_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_statebytes

    function crypto_sign_bytes() &
    bind(c, name='crypto_sign_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_bytes

    function crypto_sign_seedbytes() &
    bind(c, name='crypto_sign_seedbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_seedbytes

    function crypto_sign_publickeybytes() &
    bind(c, name='crypto_sign_publickeybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_publickeybytes

    function crypto_sign_secretkeybytes() &
    bind(c, name='crypto_sign_secretkeybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_secretkeybytes

    function crypto_sign_messagebytes_max() &
    bind(c, name='crypto_sign_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_sign_messagebytes_max

    function bind_crypto_sign_primitive() &
    bind(c, name='crypto_sign_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_sign_primitive

    function crypto_sign_seed_keypair(pk, sk, seed) &
    bind(c, name='crypto_sign_seed_keypair') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) pk, sk
      character(kind=c_char) seed
    end function crypto_sign_seed_keypair

    function crypto_sign_keypair(pk, sk) &
    bind(c, name='crypto_sign_keypair') &
    result(res)
      import c_char, c_int
      integer(kind=c_int) res
      character(kind=c_char) pk, sk
    end function crypto_sign_keypair

    function crypto_sign(sm, smlen_p, m, mlen, sk) &
    bind(c, name='crypto_sign') &
    result(res)
      import c_int, c_long_long, c_char
      integer(kind=c_int) res
      integer(kind=c_long_long) smlen_p
      character(kind=c_char) sm
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) m, sk
    end function crypto_sign

    function crypto_sign_open(m, mlen_p, sm, smlen, pk) &
    bind(c, name='crypto_sign_open') &
    result(res)
      import c_int, c_long_long, c_char
      integer(kind=c_int) res
      integer(kind=c_long_long) mlen_p
      character(kind=c_char) m, sm
      integer(kind=c_long_long), value :: smlen
      character(kind=c_char) pk
    end function crypto_sign_open

    function crypto_sign_detached(sig, siglen_p, m, mlen, sk) &
    bind(c, name='crypto_sign_detached') &
    result(res)
      import c_int, c_long_long, c_char
      integer(kind=c_int) res
      integer(kind=c_long_long) siglen_p
      character(kind=c_char) sig, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) sk
    end function crypto_sign_detached

    function crypto_sign_verify_detached(sig, m, mlen, pk) &
    bind(c, name='crypto_sign_verify_detached') &
    result(res)
      import c_int, c_long_long, c_char
      integer(kind=c_int) res
      character(kind=c_char) sig, m
      integer(kind=c_long_long), value :: mlen
      character(kind=c_char) pk
    end function crypto_sign_verify_detached

    function crypto_sign_init(state) &
    bind(c, name='crypto_sign_init') &
    result(res)
      import c_int, crypto_sign_state
      integer(kind=c_int) res
      type(crypto_sign_state) state
    end function crypto_sign_init

    function crypto_sign_update(state, m, mlen) &
    bind(c, name='crypto_sign_update') &
    result(res)
      import c_int, crypto_sign_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_sign_state) state
      character(kind=c_char) m
      integer(kind=c_long_long), value :: mlen
    end function crypto_sign_update

    function crypto_sign_final_create(state, sig, siglen_p, sk) &
    bind(c, name='crypto_sign_final_create') &
    result(res)
      import c_int, crypto_sign_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_sign_state) state
      character(kind=c_char) sig
      integer(kind=c_long_long) siglen_p
      character(kind=c_char) sk
    end function crypto_sign_final_create

    function crypto_sign_final_verify(state, sig, pk) &
    bind(c, name='crypto_sign_final_verify') &
    result(res)
      import c_int, crypto_sign_state, c_char
      integer(kind=c_int) res
      type(crypto_sign_state) state
      character(kind=c_char) sig
      character(kind=c_char) pk
    end function crypto_sign_final_verify

  end interface

contains

  function crypto_sign_primitive() result(res)
    type(c_ptr) res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_sign_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_sign_primitive

end module mod_crypto_sign
