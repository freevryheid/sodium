module mod_crypto_aead_chacha20poly1305
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: crypto_aead_chacha20poly1305_ietf_keybytes
  public :: crypto_aead_chacha20poly1305_ietf_nsecbytes
  public :: crypto_aead_chacha20poly1305_ietf_npubbytes
  public :: crypto_aead_chacha20poly1305_ietf_abytes
  public :: crypto_aead_chacha20poly1305_ietf_messagebytes_max
  public :: crypto_aead_chacha20poly1305_ietf_encrypt
  public :: crypto_aead_chacha20poly1305_ietf_decrypt
  public :: crypto_aead_chacha20poly1305_ietf_encrypt_detached
  public :: crypto_aead_chacha20poly1305_ietf_decrypt_detached
  public :: crypto_aead_chacha20poly1305_ietf_keygen
  public :: crypto_aead_chacha20poly1305_keybytes
  public :: crypto_aead_chacha20poly1305_nsecbytes
  public :: crypto_aead_chacha20poly1305_npubbytes
  public :: crypto_aead_chacha20poly1305_abytes
  public :: crypto_aead_chacha20poly1305_messagebytes_max
  public :: crypto_aead_chacha20poly1305_encrypt
  public :: crypto_aead_chacha20poly1305_decrypt
  public :: crypto_aead_chacha20poly1305_encrypt_detached
  public :: crypto_aead_chacha20poly1305_decrypt_detached
  public :: crypto_aead_chacha20poly1305_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ietf_KEYBYTES         = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ietf_NSECBYTES        = 0
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ietf_NPUBBYTES        = 12
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ietf_ABYTES           = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ietf_MESSAGEBYTES_MAX = &
    (SODIUM_SIZE_MAX - SODIUM_crypto_aead_chacha20poly1305_ietf_ABYTES)
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_KEYBYTES              = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_NSECBYTES             = 0
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_NPUBBYTES             = 8
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_chacha20poly1305_ABYTES                = 16

  interface

    function crypto_aead_chacha20poly1305_ietf_keybytes() &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_ietf_keybytes

    function crypto_aead_chacha20poly1305_ietf_nsecbytes() &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_nsecbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_ietf_nsecbytes

    function crypto_aead_chacha20poly1305_ietf_npubbytes() &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_npubbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_ietf_npubbytes

    function crypto_aead_chacha20poly1305_ietf_abytes() &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_abytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_ietf_abytes

    function crypto_aead_chacha20poly1305_ietf_messagebytes_max() &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_ietf_messagebytes_max

    function crypto_aead_chacha20poly1305_ietf_encrypt(c, clen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_encrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: clen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_ietf_encrypt

    function crypto_aead_chacha20poly1305_ietf_decrypt(m, mlen_p, nsec, c, clen, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_decrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_ietf_decrypt

    function crypto_aead_chacha20poly1305_ietf_encrypt_detached(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_encrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: maclen_p
      character(kind=c_char) :: mac, m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_ietf_encrypt_detached

    function crypto_aead_chacha20poly1305_ietf_decrypt_detached(m, nsec, c, clen, mac, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_decrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_ietf_decrypt_detached

    subroutine crypto_aead_chacha20poly1305_ietf_keygen(k) &
    bind(c, name='crypto_aead_chacha20poly1305_ietf_keygen')
      import :: c_char
      character(kind=c_char) :: k
    end subroutine crypto_aead_chacha20poly1305_ietf_keygen

    function crypto_aead_chacha20poly1305_keybytes() &
    bind(c, name='crypto_aead_chacha20poly1305_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_keybytes

    function crypto_aead_chacha20poly1305_nsecbytes() &
    bind(c, name='crypto_aead_chacha20poly1305_nsecbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_nsecbytes

    function crypto_aead_chacha20poly1305_npubbytes() &
    bind(c, name='crypto_aead_chacha20poly1305_npubbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_npubbytes

    function crypto_aead_chacha20poly1305_abytes() &
    bind(c, name='crypto_aead_chacha20poly1305_abytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_abytes

    function crypto_aead_chacha20poly1305_messagebytes_max() &
    bind(c, name='crypto_aead_chacha20poly1305_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_chacha20poly1305_messagebytes_max

    function crypto_aead_chacha20poly1305_encrypt(c, clen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_encrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: clen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_encrypt

    function crypto_aead_chacha20poly1305_decrypt(m, mlen_p, nsec, c, clen, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_decrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_decrypt

    function crypto_aead_chacha20poly1305_encrypt_detached(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_encrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: maclen_p
      character(kind=c_char) :: mac, m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_encrypt_detached

    function crypto_aead_chacha20poly1305_decrypt_detached(m, nsec, c, clen, mac, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_chacha20poly1305_decrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function crypto_aead_chacha20poly1305_decrypt_detached

    subroutine crypto_aead_chacha20poly1305_keygen(k) &
    bind(c, name='crypto_aead_chacha20poly1305_keygen')
      import :: c_char
      character(kind=c_char) :: k
    end subroutine crypto_aead_chacha20poly1305_keygen

  end interface

end module mod_crypto_aead_chacha20poly1305