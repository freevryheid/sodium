module mod_crypto_auth_hmacsha512
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_long_long, c_int
  use mod_crypto_hash_sha512
  implicit none
  private

  public crypto_auth_hmacsha512_bytes
  public crypto_auth_hmacsha512_keybytes
  public crypto_auth_hmacsha512
  public crypto_auth_hmacsha512_verify
  public crypto_auth_hmacsha512_statebytes
  public crypto_auth_hmacsha512_init
  public crypto_auth_hmacsha512_update
  public crypto_auth_hmacsha512_final
  public crypto_auth_hmacsha512_keygen

  integer, parameter, public :: SODIUM_crypto_auth_hmacsha512_BYTES    = 64
  integer, parameter, public :: SODIUM_crypto_auth_hmacsha512_KEYBYTES = 32

  type, public, bind(c) :: crypto_auth_hmacsha512_state
    type(crypto_hash_sha512_state) ictx
    type(crypto_hash_sha512_state) octx
  end type

  interface

    function crypto_auth_hmacsha512_bytes() &
    bind(c, name='crypto_auth_hmacsha512_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_auth_hmacsha512_bytes

    function crypto_auth_hmacsha512_keybytes() &
    bind(c, name='crypto_auth_hmacsha512_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_auth_hmacsha512_keybytes

    function crypto_auth_hmacsha512(out, in, inlen, k) &
    bind(c, name='crypto_auth_hmacsha512') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function crypto_auth_hmacsha512

    function crypto_auth_hmacsha512_verify(h, in, inlen, k) &
    bind(c, name='crypto_auth_hmacsha512_verify') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) h
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function crypto_auth_hmacsha512_verify

    function crypto_auth_hmacsha512_statebytes() &
    bind(c, name='crypto_auth_hmacsha512_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_auth_hmacsha512_statebytes

    function crypto_auth_hmacsha512_init(state, key, keylen) &
    bind(c, name='crypto_auth_hmacsha512_init') &
    result(res)
      import c_char, c_int, c_size_t, crypto_auth_hmacsha512_state
      integer(kind=c_int) res
      type(crypto_auth_hmacsha512_state) state
      character(kind=c_char) key
      integer(kind=c_size_t), value :: keylen
    end function crypto_auth_hmacsha512_init

    function crypto_auth_hmacsha512_update(state, in, inlen) &
    bind(c, name='crypto_auth_hmacsha512_update') &
    result(res)
      import c_int, crypto_auth_hmacsha512_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_auth_hmacsha512_state) state
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function crypto_auth_hmacsha512_update

    function crypto_auth_hmacsha512_final(state, out) &
    bind(c, name='crypto_auth_hmacsha512_final') &
    result(res)
      import c_int, c_char, crypto_auth_hmacsha512_state
      integer(kind=c_int) res
      type(crypto_auth_hmacsha512_state) state
      character(kind=c_char) out
    end function crypto_auth_hmacsha512_final

    subroutine crypto_auth_hmacsha512_keygen(k) &
    bind(c, name='crypto_auth_hmacsha512_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_auth_hmacsha512_keygen

  end interface

end module mod_crypto_auth_hmacsha512
