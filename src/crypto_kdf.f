module mod_crypto_kdf
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char, c_size_t, c_int64_t
  use mod_common
  use mod_crypto_kdf_blake2b
  implicit none
  private

  public crypto_kdf_bytes_min
  public crypto_kdf_bytes_max
  public crypto_kdf_contextbytes
  public crypto_kdf_keybytes
  public crypto_kdf_primitive
  public crypto_kdf_derive_from_key
  public crypto_kdf_keygen

  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_BYTES_MIN    = SODIUM_crypto_kdf_blake2b_BYTES_MIN
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_BYTES_MAX    = SODIUM_crypto_kdf_blake2b_BYTES_MAX
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_CONTEXTBYTES = SODIUM_crypto_kdf_blake2b_CONTEXTBYTES
  integer(kind=c_size_t), paremeter, public :: SODIUM_crypto_kdf_KEYBYTES     = SODIUM_crypto_kdf_blake2b_KEYBYTES
  character(len=*), parameter, public :: SODIUM_crypto_kdf_PRIMITIVE          = "blake2b"

  interface

    function crypto_kdf_bytes_min() &
    bind(c, name='crypto_kdf_bytes_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_bytes_min

    function crypto_kdf_bytes_max() &
    bind(c, name='crypto_kdf_bytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_bytes_max

    function crypto_kdf_contextbytes() &
    bind(c, name='crypto_kdf_contextbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_contextbytes

    function crypto_kdf_keybytes() &
    bind(c, name='crypto_kdf_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_kdf_keybytes

    function bind_crypto_kdf_primitive() &
    bind(c, name='crypto_kdf_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_kdf_primitive

    ! TODO: testme
    function crypto_kdf_derive_from_key(subkey, subkey_len, subkey_id, ctx, key) &
    bind(c, name='crypto_kdf_derive_from_key') &
    result(res)
      import c_int, c_char, c_size_t, c_int64_t
      integer(kind=c_int) res
      character(kind=c_char) subkey
      integer(kind=c_size_t), value :: subkey_len
      integer(kind=c_int64_t), value :: subkey_id
      character(kind=c_char) ctx
      character(kind=c_char) key
    end function crypto_kdf_derive_from_key

    subroutine crypto_kdf_keygen(k) &
    bind(c, name='crypto_kdf_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_kdf_keygen

  end interface

contains

  function crypto_kdf_primitive() result(res)
    type(c_ptr) res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_kdf_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_kdf_primitive

end module mod_crypto_kdf
