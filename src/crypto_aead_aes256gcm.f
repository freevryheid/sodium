module mod_crypto_aead_aes256gcm
  use, intrinsic :: iso_c_binding, only : c_int, c_size_t, c_char, c_long_long, c_null_char
  use mod_core
  implicit none
  private

  public crypto_aead_aes256gcm_is_available
  public crypto_aead_aes256gcm_keybytes
  public crypto_aead_aes256gcm_nsecbytes
  public crypto_aead_aes256gcm_npubbytes
  public crypto_aead_aes256gcm_abytes
  public crypto_aead_aes256gcm_messagebytes_max
  public crypto_aead_aes256gcm_statebytes
  public crypto_aead_aes256gcm_encrypt
  public crypto_aead_aes256gcm_decrypt
  public crypto_aead_aes256gcm_encrypt_detached
  public crypto_aead_aes256gcm_decrypt_detached
  public crypto_aead_aes256gcm_beforenm
  public crypto_aead_aes256gcm_encrypt_afternm
  public crypto_aead_aes256gcm_decrypt_afternm
  public crypto_aead_aes256gcm_encrypt_detached_afternm
  public crypto_aead_aes256gcm_decrypt_detached_afternm
  public crypto_aead_aes256gcm_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aes256gcm_KEYBYTES  = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aes256gcm_NSECBYTES = 0
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aes256gcm_NPUBBYTES = 12
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aes256gcm_ABYTES    = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_aead_aes256gcm_MESSAGEBYTES_MAX = &
    (SODIUM_SIZE_MAX - SODIUM_crypto_aead_aes256gcm_ABYTES)

  type, bind(c) :: aead
    character(kind=c_char) :: opaque(512)
  end type

  type, public, bind(c) :: crypto_aead_aes256gcm_state
    type(aead) :: crypto_align(16)
  end type

  interface

    function crypto_aead_aes256gcm_is_available() &
    bind(c, name='crypto_aead_aes256gcm_is_available') &
    result(res)
      import c_int
      integer(kind=c_int) res
    end function crypto_aead_aes256gcm_is_available

    function crypto_aead_aes256gcm_keybytes() &
    bind(c, name='crypto_aead_aes256gcm_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_keybytes

    function crypto_aead_aes256gcm_nsecbytes() &
    bind(c, name='crypto_aead_aes256gcm_nsecbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_nsecbytes

    function crypto_aead_aes256gcm_npubbytes() &
    bind(c, name='crypto_aead_aes256gcm_npubbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_npubbytes

    function crypto_aead_aes256gcm_abytes() &
    bind(c, name='crypto_aead_aes256gcm_abytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_abytes

    function crypto_aead_aes256gcm_messagebytes_max() &
    bind(c, name='crypto_aead_aes256gcm_messagebytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_messagebytes_max

    function crypto_aead_aes256gcm_statebytes() &
    bind(c, name='crypto_aead_aes256gcm_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_aead_aes256gcm_statebytes

    function bind_crypto_aead_aes256gcm_encrypt(c, clen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_aes256gcm_encrypt') &
    result(res)
      import c_int, c_char, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) clen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) nsec, npub, k
    end function bind_crypto_aead_aes256gcm_encrypt

    function bind_crypto_aead_aes256gcm_decrypt(m, mlen_p, nsec, c, clen, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_aes256gcm_decrypt') &
    result(res)
      import c_int, c_char, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) mlen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) nsec, npub, k
    end function bind_crypto_aead_aes256gcm_decrypt

    function bind_crypto_aead_aes256gcm_encrypt_detached(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_aes256gcm_encrypt_detached') &
    result(res)
      import c_int, c_char, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) maclen_p
      character(kind=c_char) mac, m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) nsec, npub, k
    end function bind_crypto_aead_aes256gcm_encrypt_detached

    function bind_crypto_aead_aes256gcm_decrypt_detached(m, nsec, c, clen, mac, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_aes256gcm_decrypt_detached') &
    result(res)
      import c_int, c_char, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) c, mac
      integer(kind=c_long_long) mlen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) nsec, npub, k
    end function bind_crypto_aead_aes256gcm_decrypt_detached

    function crypto_aead_aes256gcm_beforenm(ctx_, k) &
    bind(c, name='crypto_aead_aes256gcm_beforenm') &
    result(res)
      import c_int, c_char, crypto_aead_aes256gcm_state
      integer(kind=c_int) res
      type(crypto_aead_aes256gcm_state) ctx_
      character(kind=c_char) k
    end function crypto_aead_aes256gcm_beforenm

    function crypto_aead_aes256gcm_encrypt_afternm(c, clen_p, m, mlen, ad, adlen, nsec, npub, ctx_) &
    bind(c, name='crypto_aead_aes256gcm_encrypt_afternm') &
    result(res)
      import c_int, c_char, c_long_long, crypto_aead_aes256gcm_state
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) clen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) nsec, npub
      type(crypto_aead_aes256gcm_state) ctx_
    end function crypto_aead_aes256gcm_encrypt_afternm

    function crypto_aead_aes256gcm_decrypt_afternm(m, mlen_p, nsec, c, clen, ad, adlen, npub, ctx_) &
    bind(c, name='crypto_aead_aes256gcm_decrypt_afternm') &
    result(res)
      import c_int, c_char, c_long_long, crypto_aead_aes256gcm_state
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) mlen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) nsec, npub
      type(crypto_aead_aes256gcm_state) ctx_
    end function crypto_aead_aes256gcm_decrypt_afternm

    function crypto_aead_aes256gcm_encrypt_detached_afternm(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, ctx_) &
    bind(c, name='crypto_aead_aes256gcm_encrypt_detached_afternm') &
    result(res)
      import c_int, c_char, c_long_long, crypto_aead_aes256gcm_state
      integer(kind=c_int) res
      character(kind=c_char) c
      integer(kind=c_long_long) maclen_p
      character(kind=c_char) mac, m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) nsec, npub
      type(crypto_aead_aes256gcm_state) ctx_
    end function crypto_aead_aes256gcm_encrypt_detached_afternm

    function crypto_aead_aes256gcm_decrypt_detached_afternm(m, nsec, c, clen, mac, ad, adlen, npub, ctx_) &
    bind(c, name='crypto_aead_aes256gcm_decrypt_detached_afternm') &
    result(res)
      import c_int, c_char, c_long_long, crypto_aead_aes256gcm_state
      integer(kind=c_int) res
      character(kind=c_char) c, mac
      integer(kind=c_long_long) mlen_p
      character(kind=c_char) m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) nsec, npub
      type(crypto_aead_aes256gcm_state) ctx_
    end function crypto_aead_aes256gcm_decrypt_detached_afternm

    subroutine crypto_aead_aes256gcm_keygen(k) &
    bind(c, name='crypto_aead_aes256gcm_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_aead_aes256gcm_keygen

  end interface

  contains

    function crypto_aead_aes256gcm_encrypt(c, m, npub, k, ad) result(res)
      integer(kind=c_int) res
      character(len=*) c, m, npub, k
      integer(kind=c_long_long) clen_p, mlen, adlen
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      character(len=*), parameter :: nsec = c_null_char
      clen_p = len(c)
      mlen = len(m)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_aead_aes256gcm_encrypt(c, clen_p, m, mlen, ad1, adlen, nsec, npub, k)
    end function crypto_aead_aes256gcm_encrypt

    function crypto_aead_aes256gcm_decrypt(m, c, npub, k, ad) result(res)
      integer(kind=c_int) res
      character(len=*) c, m, npub, k
      integer(kind=c_long_long) mlen_p, clen, adlen
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      character(len=*), parameter :: nsec = c_null_char
      mlen_p = len(m)
      clen = len(c)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_aead_aes256gcm_decrypt(m, mlen_p, nsec, c, clen, ad1, adlen, npub, k)
    end function crypto_aead_aes256gcm_decrypt

    function crypto_aead_aes256gcm_encrypt_detached(c, mac, m, npub, k, ad) result(res)
      integer(kind=c_int) res
      character(len=*) c, mac, m, npub, k
      integer(kind=c_long_long) maclen_p ! not used
      integer(kind=c_long_long) mlen, adlen
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      character(len=*), parameter :: nsec = c_null_char
      mlen = len(m)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_aead_aes256gcm_encrypt_detached(c, mac, maclen_p, m, mlen, ad1, adlen, nsec, npub, k)
    end function crypto_aead_aes256gcm_encrypt_detached

    function crypto_aead_aes256gcm_decrypt_detached(m, c, mac, npub, k, ad) result(res)
      integer(kind=c_int) res
      character(len=*) c, mac, m, npub, k
      integer(kind=c_long_long) clen, adlen
      character(len=*), optional :: ad
      character(len=:), allocatable :: ad1
      character(len=*), parameter :: nsec = c_null_char
      clen = len(c)
      if (present(ad)) then
        ad1 = ad
        adlen = len(ad)
      else
        ad1 = c_null_char
        adlen = 0
      end if
      res = bind_crypto_aead_aes256gcm_decrypt_detached(m, nsec, c, clen, mac, ad1, adlen, npub, k)
    end function crypto_aead_aes256gcm_decrypt_detached

end module mod_crypto_aead_aes256gcm
