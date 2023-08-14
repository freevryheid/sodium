module mod_crypto_kdf_hkdf_sha512
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char
  use mod_crypto_auth_hmacsha512
  implicit none
  private

  public crypto_kdf_hkdf_sha512_bytes_min
  public crypto_kdf_hkdf_sha512_bytes_max
  public crypto_kdf_hkdf_sha512_keybytes
  public crypto_kdf_hkdf_sha512_keygen
  public crypto_kdf_hkdf_sha512_statebytes
  public crypto_kdf_hkdf_sha512_extract_init
  public crypto_kdf_hkdf_sha512_extract_update
  public crypto_kdf_hkdf_sha512_extract_final
  public crypto_kdf_hkdf_sha512_keygen
  public crypto_kdf_hkdf_sha512_extract
  public crypto_kdf_hkdf_sha512_expand

  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_hkdf_sha512_BYTES_MIN = 0
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_hkdf_sha512_BYTES_MAX = &
    int(z'FF') * SODIUM_crypto_auth_hmacsha512_BYTES
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_hkdf_sha512_KEYBYTES  = &
    SODIUM_crypto_auth_hmacsha512_KEYBYTES

  type, public, bind(c) :: crypto_kdf_hkdf_sha512_state
    type(crypto_auth_hmacsha512_state) st
  end type

  interface

    function crypto_kdf_hkdf_sha512_keybytes() &
    bind(c, name='crypto_kdf_hkdf_sha512_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_hkdf_sha512_keybytes

    function crypto_kdf_hkdf_sha512_bytes_min() &
    bind(c, name='crypto_kdf_hkdf_sha512_bytes_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_hkdf_sha512_bytes_min

    function crypto_kdf_hkdf_sha512_bytes_max() &
    bind(c, name='crypto_kdf_hkdf_sha512_bytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_hkdf_sha512_bytes_max

    subroutine crypto_kdf_hkdf_sha512_keygen(k) &
    bind(c, name='crypto_kdf_hkdf_sha512_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_kdf_hkdf_sha512_keygen

    function crypto_kdf_hkdf_sha512_statebytes() &
    bind(c, name='crypto_kdf_hkdf_sha512_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_hkdf_sha512_statebytes

    function crypto_kdf_hkdf_sha512_extract_init(state, key, keylen) &
    bind(c, name='crypto_kdf_hkdf_sha512_extract_init') &
    result(res)
      import c_char, c_int, c_size_t, crypto_kdf_hkdf_sha512_state
      integer(kind=c_int) res
      type(crypto_kdf_hkdf_sha512_state) state
      character(kind=c_char) key
      integer(kind=c_size_t), value :: keylen
    end function crypto_kdf_hkdf_sha512_extract_init

    function crypto_kdf_hkdf_sha512_extract_update(state, in, inlen) &
    bind(c, name='crypto_kdf_hkdf_sha512_extract_update') &
    result(res)
      import c_int, crypto_kdf_hkdf_sha512_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_kdf_hkdf_sha512_state) state
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function crypto_kdf_hkdf_sha512_extract_update

    function crypto_kdf_hkdf_sha512_extract_final(state, out) &
    bind(c, name='crypto_kdf_hkdf_sha512_extract_final') &
    result(res)
      import c_int, c_char, crypto_kdf_hkdf_sha512_state
      integer(kind=c_int) res
      type(crypto_kdf_hkdf_sha512_state) state
      character(kind=c_char) out
    end function crypto_kdf_hkdf_sha512_extract_final

    function crypto_kdf_hkdf_sha512_extract(prk, salt, salt_len, ikm, ikm_len) &
    bind(c, name='crypto_kdf_hkdf_sha512_extract') &
    result(res)
      import c_int, c_char, c_size_t
      integer(kind=c_int) res
      integer(kind=c_size_t), value :: salt_len, ikm_len
      character(kind=c_char) prk, salt, ikm
    end function crypto_kdf_hkdf_sha512_extract

    function crypto_kdf_hkdf_sha512_expand(out, out_len, ctx, ctx_len, prk)
    bind(c, name='crypto_kdf_hkdf_sha512_expand') &
    result(res)
      import c_int, c_cha, c_size_tr
      integer(kind=c_int) res
      integer(kind=c_size_t), value :: out_len, ctx_len
      character(kind=c_char) out, ctx, prk
    end function crypto_kdf_hkdf_sha512_expand

  end interface

end module mod_crypto_kdf_hkdf_sha512SODIUM_EXPORT
