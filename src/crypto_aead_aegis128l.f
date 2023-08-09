module mod_crypto_aead_aegis128l
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char, c_long_long, c_null_char
  use :: mod_core
  implicit none
  private

  public :: crypto_aead_aegis128l_keybytes
  public :: crypto_aead_aegis128l_nsecbytes
  public :: crypto_aead_aegis128l_npubbytes
  public :: crypto_aead_aegis128l_abytes
  public :: crypto_aead_aegis128l_messagebytes_max
  public :: crypto_aead_aegis128l_encrypt
  public :: crypto_aead_aegis128l_decrypt
  public :: crypto_aead_aegis128l_encrypt_detached
  public :: crypto_aead_aegis128l_decrypt_detached
  public :: crypto_aead_aegis128l_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aegis128l_KEYBYTES         = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aegis128l_NSECBYTES        = 0
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aegis128l_NPUBBYTES        = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aegis128l_ABYTES           = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_aead_aegis128l_MESSAGEBYTES_MAX = &
    (SODIUM_SIZE_MAX - SODIUM_crypto_aead_aegis128l_ABYTES)

  interface

    function crypto_aead_aegis128l_keybytes() &
    bind(c, name='crypto_aead_aegis128l_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_aegis128l_keybytes

    function crypto_aead_aegis128l_nsecbytes() &
    bind(c, name='crypto_aead_aegis128l_nsecbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_aegis128l_nsecbytes

    function crypto_aead_aegis128l_npubbytes() &
    bind(c, name='crypto_aead_aegis128l_npubbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_aegis128l_npubbytes

    function crypto_aead_aegis128l_abytes() &
    bind(c, name='crypto_aead_aegis128l_abytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_aegis128l_abytes

    function crypto_aead_aegis128l_messagebytes_max() &
    bind(c, name='crypto_aead_aegis128l_messagebytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_aead_aegis128l_messagebytes_max

    function bind_crypto_aead_aegis128l_encrypt(c, clen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_aegis128l_encrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: clen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function bind_crypto_aead_aegis128l_encrypt

    function bind_crypto_aead_aegis128l_decrypt(m, mlen_p, nsec, c, clen, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_aegis128l_decrypt') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function bind_crypto_aead_aegis128l_decrypt

    function bind_crypto_aead_aegis128l_encrypt_detached(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, k) &
    bind(c, name='crypto_aead_aegis128l_encrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c
      integer(kind=c_long_long) :: maclen_p
      character(kind=c_char) :: mac, m, ad
      integer(kind=c_long_long), value :: mlen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function bind_crypto_aead_aegis128l_encrypt_detached

    function bind_crypto_aead_aegis128l_decrypt_detached(m, nsec, c, clen, mac, ad, adlen, npub, k) &
    bind(c, name='crypto_aead_aegis128l_decrypt_detached') &
    result(res)
      import :: c_int, c_char, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: c, mac
      integer(kind=c_long_long) :: mlen_p
      character(kind=c_char) :: m, ad
      integer(kind=c_long_long), value :: clen, adlen
      character(kind=c_char) :: nsec, npub, k
    end function bind_crypto_aead_aegis128l_decrypt_detached

    subroutine crypto_aead_aegis128l_keygen(k) &
    bind(c, name='crypto_aead_aegis128l_keygen')
      import :: c_char
      character(kind=c_char) :: k
    end subroutine crypto_aead_aegis128l_keygen

  end interface

  contains

    ! function crypto_aead_aegis128l_encrypt(c, clen_p, m, mlen, ad, adlen, nsec, npub, k) &
    function crypto_aead_aegis128l_encrypt(c, m, npub, k, ad) result(res)
      integer(kind=c_int) :: res
      character(len=*) :: c, m, npub, k
      integer(kind=c_long_long) :: clen_p, mlen, adlen
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
      res = bind_crypto_aead_aegis128l_encrypt(c, clen_p, m, mlen, ad1, adlen, nsec, npub, k)
    end function crypto_aead_aegis128l_encrypt

    ! function crypto_aead_aegis128l_decrypt(m, mlen_p, nsec, c, clen, ad, adlen, npub, k) &
    function crypto_aead_aegis128l_decrypt(m, c, npub, k, ad) result(res)
      integer(kind=c_int) :: res
      character(len=*) :: c, m, npub, k
      integer(kind=c_long_long) :: mlen_p, clen, adlen
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
      res = bind_crypto_aead_aegis128l_decrypt(m, mlen_p, nsec, c, clen, ad1, adlen, npub, k)
    end function crypto_aead_aegis128l_decrypt

    ! function crypto_aead_aegis128l_encrypt_detached(c, mac, maclen_p, m, mlen, ad, adlen, nsec, npub, k) &
    function crypto_aead_aegis128l_encrypt_detached(c, mac, m, npub, k, ad) result(res)
      integer(kind=c_int) :: res
      character(len=*) :: c, mac, m, npub, k
      integer(kind=c_long_long) :: maclen_p ! not used
      integer(kind=c_long_long) :: mlen, adlen
      character(len=*), optional:: ad
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
      res = bind_crypto_aead_aegis128l_encrypt_detached(c, mac, maclen_p, m, mlen, ad1, adlen, nsec, npub, k)
    end function crypto_aead_aegis128l_encrypt_detached

    ! function crypto_aead_aegis128l_decrypt_detached(m, nsec, c, clen, mac, ad, adlen, npub, k) &
    function crypto_aead_aegis128l_decrypt_detached(m, c, mac, npub, k, ad) result(res)
      integer(kind=c_int) :: res
      character(len=*) :: c, mac, m, npub, k
      integer(kind=c_long_long) :: clen, adlen
      character(len=*), optional:: ad
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
      res = bind_crypto_aead_aegis128l_decrypt_detached(m, nsec, c, clen, mac, ad1, adlen, npub, k)
    end function crypto_aead_aegis128l_decrypt_detached

end module mod_crypto_aead_aegis128l
