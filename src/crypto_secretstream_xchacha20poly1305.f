module mod_crypto_secretstream_xchacha20poly1305
  use, intrinsic :: iso_c_binding, only : c_size_t, c_signed_char, c_int, c_char, c_long_long, c_null_char
  use mod_crypto_aead_xchacha20poly1305
  use mod_crypto_stream_chacha20
  use mod_core
  implicit none
  private

  type, public, bind(c) :: crypto_secretstream_xchacha20poly1305_state
    character(kind=c_char) k(SODIUM_crypto_stream_chacha20_ietf_KEYBYTES)
    character(kind=c_char) nonce(SODIUM_crypto_stream_chacha20_ietf_NONCEBYTES)
    character(kind=c_char) pad(8)
  end type

  public crypto_secretstream_xchacha20poly1305_abytes
  public crypto_secretstream_xchacha20poly1305_headerbytes
  public crypto_secretstream_xchacha20poly1305_keybytes
  public crypto_secretstream_xchacha20poly1305_messagebytes_max
  public crypto_secretstream_xchacha20poly1305_tag_message
  public crypto_secretstream_xchacha20poly1305_tag_push
  public crypto_secretstream_xchacha20poly1305_tag_rekey
  public crypto_secretstream_xchacha20poly1305_tag_final
  public crypto_secretstream_xchacha20poly1305_statebytes
  public crypto_secretstream_xchacha20poly1305_keygen
  public crypto_secretstream_xchacha20poly1305_init_push
  public crypto_secretstream_xchacha20poly1305_push
  public crypto_secretstream_xchacha20poly1305_init_pull
  public crypto_secretstream_xchacha20poly1305_pull
  public crypto_secretstream_xchacha20poly1305_rekey

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretstream_xchacha20poly1305_ABYTES           = &
    (1 + SODIUM_crypto_aead_xchacha20poly1305_ietf_ABYTES)
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretstream_xchacha20poly1305_HEADERBYTES      = &
    SODIUM_crypto_aead_xchacha20poly1305_ietf_NPUBBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretstream_xchacha20poly1305_KEYBYTES         = &
    SODIUM_crypto_aead_xchacha20poly1305_ietf_KEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_secretstream_xchacha20poly1305_MESSAGEBYTES_MAX = &
    (SODIUM_SIZE_MAX - SODIUM_crypto_secretstream_xchacha20poly1305_ABYTES)
  integer(kind=c_signed_char), public, parameter :: SODIUM_crypto_secretstream_xchacha20poly1305_TAG_MESSAGE = 0
  integer(kind=c_signed_char), public, parameter :: SODIUM_crypto_secretstream_xchacha20poly1305_TAG_PUSH    = 1
  integer(kind=c_signed_char), public, parameter :: SODIUM_crypto_secretstream_xchacha20poly1305_TAG_REKEY   = 2
  integer(kind=c_signed_char), public, parameter :: SODIUM_crypto_secretstream_xchacha20poly1305_TAG_FINAL   = &
    ior(SODIUM_crypto_secretstream_xchacha20poly1305_TAG_PUSH, SODIUM_crypto_secretstream_xchacha20poly1305_TAG_REKEY)

  interface

    function crypto_secretstream_xchacha20poly1305_abytes() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_abytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretstream_xchacha20poly1305_abytes

    function crypto_secretstream_xchacha20poly1305_headerbytes() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_headerbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretstream_xchacha20poly1305_headerbytes

    function crypto_secretstream_xchacha20poly1305_keybytes() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretstream_xchacha20poly1305_keybytes

    function crypto_secretstream_xchacha20poly1305_messagebytes_max() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretstream_xchacha20poly1305_messagebytes_max

    function crypto_secretstream_xchacha20poly1305_tag_message() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_tag_message') &
    result(res)
      import c_signed_char
      integer(kind=c_signed_char) res
    end function crypto_secretstream_xchacha20poly1305_tag_message

    function crypto_secretstream_xchacha20poly1305_tag_push() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_tag_push') &
    result(res)
      import c_signed_char
      integer(kind=c_signed_char) res
    end function crypto_secretstream_xchacha20poly1305_tag_push

    function crypto_secretstream_xchacha20poly1305_tag_rekey() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_tag_rekey') &
    result(res)
      import c_signed_char
      integer(kind=c_signed_char) res
    end function crypto_secretstream_xchacha20poly1305_tag_rekey

    function crypto_secretstream_xchacha20poly1305_tag_final() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_tag_final') &
    result(res)
      import c_signed_char
      integer(kind=c_signed_char) res
    end function crypto_secretstream_xchacha20poly1305_tag_final

    function crypto_secretstream_xchacha20poly1305_statebytes() &
    bind(c, name='crypto_secretstream_xchacha20poly1305_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_secretstream_xchacha20poly1305_statebytes

    subroutine crypto_secretstream_xchacha20poly1305_keygen(k) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_secretstream_xchacha20poly1305_keygen

    function crypto_secretstream_xchacha20poly1305_init_push(state, header, k) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_init_push') &
    result(res)
      import c_int, c_char, crypto_secretstream_xchacha20poly1305_state
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      character(kind=c_char) header
      character(kind=c_char) k
    end function crypto_secretstream_xchacha20poly1305_init_push

    function bind_crypto_secretstream_xchacha20poly1305_push(state, c, clen, m, mlen, ad, adlen, tag) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_push') &
    result(res)
      import c_int, c_char, c_long_long, crypto_secretstream_xchacha20poly1305_state, c_signed_char
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      integer(kind=c_long_long) clen
      character(kind=c_char) c
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) m, ad
      integer(kind=c_signed_char), value :: tag
    end function bind_crypto_secretstream_xchacha20poly1305_push

    function crypto_secretstream_xchacha20poly1305_init_pull(state, header, k) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_init_pull') &
    result(res)
      import c_int, c_char, crypto_secretstream_xchacha20poly1305_state
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      character(kind=c_char) header
      character(kind=c_char) k
    end function crypto_secretstream_xchacha20poly1305_init_pull

    function bind_crypto_secretstream_xchacha20poly1305_pull(state, m, mlen, tag, c, clen, ad, adlen) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_pull') &
    result(res)
      import c_int, c_char, c_long_long, crypto_secretstream_xchacha20poly1305_state, c_signed_char
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      integer(kind=c_long_long) mlen
      character(kind=c_char) m
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) c, ad
      integer(kind=c_signed_char) :: tag
    end function bind_crypto_secretstream_xchacha20poly1305_pull

    subroutine crypto_secretstream_xchacha20poly1305_rekey(state) &
    bind(c, name='crypto_secretstream_xchacha20poly1305_rekey')
      import crypto_secretstream_xchacha20poly1305_state
      type(crypto_secretstream_xchacha20poly1305_state) state
    end subroutine crypto_secretstream_xchacha20poly1305_rekey

  end interface

  contains

    function crypto_secretstream_xchacha20poly1305_push(state, c, m, tag, ad) result(res)
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      integer(kind=c_long_long) clen
      character(len=*) c
      integer(kind=c_long_long) mlen, adlen
      character(len=*) m
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      integer(kind=c_signed_char) tag
      clen = len(c)
      mlen = len(m)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_secretstream_xchacha20poly1305_push(state, c, clen, m, mlen, ad1, adlen, tag)
    end function crypto_secretstream_xchacha20poly1305_push

    function crypto_secretstream_xchacha20poly1305_pull(state, m, tag, c, ad) result(res)
      integer(kind=c_int) res
      type(crypto_secretstream_xchacha20poly1305_state) state
      integer(kind=c_long_long) mlen
      character(len=*) m
      integer(kind=c_long_long) clen, adlen
      character(len=*) c
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      integer(kind=c_signed_char) tag
      clen = len(c)
      mlen = len(m)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_secretstream_xchacha20poly1305_pull(state, m, mlen, tag, c, clen, ad1, adlen)
    end function crypto_secretstream_xchacha20poly1305_pull

end module mod_crypto_secretstream_xchacha20poly1305
