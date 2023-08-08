module mod_crypto_kdf_blake2b
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char, c_size_t, c_int64_t
  implicit none
  private

  public :: crypto_kdf_blake2b_bytes_min
  public :: crypto_kdf_blake2b_bytes_max
  public :: crypto_kdf_blake2b_contextbytes
  public :: crypto_kdf_blake2b_keybytes
  public :: crypto_kdf_blake2b_derive_from_key

  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_blake2b_BYTES_MIN    = 16
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_blake2b_BYTES_MAX    = 64
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_blake2b_CONTEXTBYTES = 8
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_blake2b_KEYBYTES     = 32

  interface

    function crypto_kdf_blake2b_bytes_min() &
    bind(c, name='crypto_kdf_blake2b_bytes_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_blake2b_bytes_min

    function crypto_kdf_blake2b_bytes_max() &
    bind(c, name='crypto_kdf_blake2b_bytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_blake2b_bytes_max

    function crypto_kdf_blake2b_contextbytes() &
    bind(c, name='crypto_kdf_blake2b_contextbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_blake2b_contextbytes

    function crypto_kdf_blake2b_keybytes() &
    bind(c, name='crypto_kdf_blake2b_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_kdf_blake2b_keybytes

    function crypto_kdf_blake2b_derive_from_key(subkey, subkey_len, subkey_id, ctx, key) &
    bind(c, name='crypto_kdf_blake2b_derive_from_key') &
    result(res)
      import :: c_int, c_char, c_size_t, c_int64_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: subkey
      integer(kind=c_size_t), value :: subkey_len
      integer(kind=c_int64_t), value :: subkey_id
      character(kind=c_char) :: ctx
      character(kind=c_char) :: key
    end function crypto_kdf_blake2b_derive_from_key

  end interface

end module mod_crypto_kdf_blake2b