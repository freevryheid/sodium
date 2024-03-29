module mod_crypto_onetimeauth
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr
  use mod_crypto_onetimeauth_poly1305
  use mod_common
  implicit none

  private

  public crypto_onetimeauth_statebytes
  public crypto_onetimeauth_bytes
  public crypto_onetimeauth_keybytes
  public crypto_onetimeauth_primitive
  public crypto_onetimeauth
  public crypto_onetimeauth_verify
  public crypto_onetimeauth_init
  public crypto_onetimeauth_update
  public crypto_onetimeauth_final
  public crypto_onetimeauth_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_onetimeauth_BYTES    = SODIUM_crypto_onetimeauth_poly1305_BYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_onetimeauth_KEYBYTES = SODIUM_crypto_onetimeauth_poly1305_KEYBYTES
  character(len=*), parameter, public :: SODIUM_crypto_onetimeauth_PRIMITIVE      = "poly1305"

  interface

    function crypto_onetimeauth_statebytes() &
    bind(c, name='crypto_onetimeauth_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_statebytes

    function crypto_onetimeauth_bytes() &
    bind(c, name='crypto_onetimeauth_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_bytes

    function crypto_onetimeauth_keybytes() &
    bind(c, name='crypto_onetimeauth_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_onetimeauth_keybytes

    function bind_crypto_onetimeauth_primitive() &
    bind(c, name='crypto_onetimeauth_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_onetimeauth_primitive

    function bind_crypto_onetimeauth(out, in, inlen, k) &
    bind(c, name='crypto_onetimeauth') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function bind_crypto_onetimeauth

    function bind_crypto_onetimeauth_verify(h, in, inlen, k) &
    bind(c, name='crypto_onetimeauth_verify') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) h
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function bind_crypto_onetimeauth_verify

    function crypto_onetimeauth_init(state, key) &
    bind(c, name='crypto_onetimeauth_init') &
    result(res)
      import c_char, c_int, crypto_onetimeauth_poly1305_state
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) key
    end function crypto_onetimeauth_init

    function crypto_onetimeauth_update(state, in, inlen) &
    bind(c, name='crypto_onetimeauth_update') &
    result(res)
      import c_int, crypto_onetimeauth_poly1305_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function crypto_onetimeauth_update

    function crypto_onetimeauth_final(state, out) &
    bind(c, name='crypto_onetimeauth_final') &
    result(res)
      import c_int, c_char, crypto_onetimeauth_poly1305_state
      integer(kind=c_int) res
      type(crypto_onetimeauth_poly1305_state) state
      character(kind=c_char) out
    end function crypto_onetimeauth_final

    subroutine crypto_onetimeauth_keygen(k) &
    bind(c, name='crypto_onetimeauth_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_onetimeauth_keygen

  end interface

  contains

    function crypto_onetimeauth_primitive() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_onetimeauth_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_onetimeauth_primitive

    function crypto_onetimeauth(out, in, k) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) in
      integer(kind=c_long_long) inlen
      character(len=*) k
      inlen = len(in)
      res = bind_crypto_onetimeauth(out, in, inlen, k)
    end function crypto_onetimeauth

    function crypto_onetimeauth_verify(h, in, k) result(res)
      integer(kind=c_int) res
      character(len=*) h
      character(len=*) in
      integer(kind=c_long_long) inlen
      character(len=*) k
      inlen = len(in)
      res = bind_crypto_onetimeauth_verify(h, in, inlen, k)
    end function crypto_onetimeauth_verify

end module mod_crypto_onetimeauth
