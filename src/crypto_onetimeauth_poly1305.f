module mod_crypto_onetimeauth_poly1305
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long
  implicit none
  private

  public crypto_onetimeauth_poly1305_statebytes
  public crypto_onetimeauth_poly1305_bytes
  public crypto_onetimeauth_poly1305_keybytes
  public crypto_onetimeauth_poly1305
  public crypto_onetimeauth_poly1305_verify
  public crypto_onetimeauth_poly1305_init
  public crypto_onetimeauth_poly1305_update
  public crypto_onetimeauth_poly1305_final
  public crypto_onetimeauth_poly1305_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_onetimeauth_poly1305_BYTES  = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_onetimeauth_poly1305_KEYBYTES  = 32

  type, bind(c) :: poly1305
    character(kind=c_char) opaque(256)
  end type

  type, public, bind(c) :: crypto_onetimeauth_poly1305_state
    type(poly1305) crypto_align(16)
  end type

  interface

    function crypto_onetimeauth_poly1305_statebytes() &
    bind(c, name='crypto_onetimeauth_poly1305_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_poly1305_statebytes

    function crypto_onetimeauth_poly1305_bytes() &
    bind(c, name='crypto_onetimeauth_poly1305_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_poly1305_bytes

    function crypto_onetimeauth_poly1305_keybytes() &
    bind(c, name='crypto_onetimeauth_poly1305_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_poly1305_keybytes

    function crypto_onetimeauth_poly1305(out, in, inlen, k) &
    bind(c, name='crypto_onetimeauth_poly1305') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function crypto_onetimeauth_poly1305

    function crypto_onetimeauth_poly1305_verify(h, in, inlen, k) &
    bind(c, name='crypto_onetimeauth_poly1305_verify') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) h
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function crypto_onetimeauth_poly1305_verify

    function crypto_onetimeauth_poly1305_init(state, key) &
    bind(c, name='crypto_onetimeauth_poly1305_init') &
    result(res)
      import c_char, c_int, crypto_onetimeauth_poly1305_state
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) key
    end function crypto_onetimeauth_poly1305_init

    function crypto_onetimeauth_poly1305_update(state, in, inlen) &
    bind(c, name='crypto_onetimeauth_poly1305_update') &
    result(res)
      import c_int, crypto_onetimeauth_poly1305_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function crypto_onetimeauth_poly1305_update

    function crypto_onetimeauth_poly1305_final(state, out) &
    bind(c, name='crypto_onetimeauth_poly1305_final') &
    result(res)
      import c_int, c_char, crypto_onetimeauth_poly1305_state
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) out
    end function crypto_onetimeauth_poly1305_final

    subroutine crypto_onetimeauth_poly1305_keygen(k) &
    bind(c, name='crypto_onetimeauth_poly1305_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_onetimeauth_poly1305_keygen

  end interface

end module mod_crypto_onetimeauth_poly1305
