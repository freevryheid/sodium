module mod_crypto_generichash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr, c_null_char
  use mod_crypto_generichash_blake2b
  use mod_common, only : c_f_str_ptr
  implicit none
  private

  public crypto_generichash_bytes_min
  public crypto_generichash_bytes_max
  public crypto_generichash_bytes
  public crypto_generichash_keybytes_min
  public crypto_generichash_keybytes_max
  public crypto_generichash_keybytes
  public crypto_generichash_primitive
  public crypto_generichash_statebytes
  public crypto_generichash
  public crypto_generichash_init
  public crypto_generichash_update
  public crypto_generichash_final
  public crypto_generichash_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_BYTES_MIN    = &
    SODIUM_crypto_generichash_blake2b_BYTES_MIN
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_BYTES_MAX    = &
    SODIUM_crypto_generichash_blake2b_BYTES_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_KEYBYTES_MAX = &
    SODIUM_crypto_generichash_blake2b_KEYBYTES_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_KEYBYTES     = &
    SODIUM_crypto_generichash_blake2b_KEYBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_BYTES        = &
    SODIUM_crypto_generichash_blake2b_BYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_generichash_KEYBYTES_MIN = &
    SODIUM_crypto_generichash_blake2b_KEYBYTES_MIN
  character(len=*), parameter, public :: SPDIUM_crypto_generichash_PRIMITIVE          = &
    "blake2b"

  interface

    function crypto_generichash_bytes_min() &
    bind(c, name='crypto_generichash_bytes_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_bytes_min

    function crypto_generichash_bytes_max() &
    bind(c, name='crypto_generichash_bytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_bytes_max

    function crypto_generichash_bytes() &
    bind(c, name='crypto_generichash_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_bytes

    function crypto_generichash_keybytes_min() &
    bind(c, name='crypto_generichash_keybytes_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_keybytes_min

    function crypto_generichash_keybytes_max() &
    bind(c, name='crypto_generichash_keybytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_keybytes_max

    function crypto_generichash_keybytes() &
    bind(c, name='crypto_generichash_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_keybytes

    function bind_crypto_generichash_primitive() &
    bind(c, name='crypto_generichash_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_generichash_primitive

    function crypto_generichash_statebytes() &
    bind(c, name='crypto_generichash_statebytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_generichash_statebytes

    function bind_crypto_generichash(out, outlen, in, inlen, key, keylen) &
    bind(c, name='crypto_generichash') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_size_t), value :: outlen, keylen
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) key
    end function bind_crypto_generichash

    function bind_crypto_generichash_init(state, key, keylen, outlen) &
    bind(c, name='crypto_generichash_init') &
    result(res)
      import c_int, crypto_generichash_blake2b_state, c_char, c_size_t
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(kind=c_char) key
      integer(kind=c_size_t), value :: outlen, keylen
    end function bind_crypto_generichash_init

    function bind_crypto_generichash_update(state, in, inlen) &
    bind(c, name='crypto_generichash_update') &
    result(res)
      import c_int, crypto_generichash_blake2b_state, c_char, c_long_long
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function bind_crypto_generichash_update

    function bind_crypto_generichash_final(state, out, outlen) &
    bind(c, name='crypto_generichash_final') &
    result(res)
      import c_int, crypto_generichash_blake2b_state, c_char, c_size_t
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(kind=c_char) out
      integer(kind=c_size_t), value :: outlen
    end function bind_crypto_generichash_final

    subroutine crypto_generichash_keygen(k) &
    bind(c, name='crypto_generichash_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_generichash_keygen

  end interface

  contains

    function crypto_generichash_primitive() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_generichash_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_generichash_primitive

    function crypto_generichash(out, in, key) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) in
      integer(kind=c_size_t) outlen, keylen
      integer(kind=c_long_long) inlen
      character(len=*), optional :: key
      character(len=:), allocatable :: key1
      outlen = len(out)
      inlen = len(in)
      if (present(key)) then
        key1 = key
        keylen = len(key)
      else
        key1 = c_null_char
        keylen = 0_c_size_t
      end if
      res = bind_crypto_generichash(out, outlen, in, inlen, key1, keylen)
    end function crypto_generichash

    function crypto_generichash_init(state, key) result(res)
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(len=*), optional :: key
      character(len=:), allocatable :: key1
      integer(kind=c_size_t) outlen, keylen
      outlen = SODIUM_crypto_generichash_BYTES
      if (present(key)) then
        key1 = key
        keylen = len(key)
      else
        key1 = c_null_char
        keylen = 0
      end if
      res = bind_crypto_generichash_init(state, key1, keylen, outlen)
    end function crypto_generichash_init

    function crypto_generichash_update(state, in) result(res)
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(len=*) in
      integer(kind=c_long_long) inlen
      inlen = len(in)
      res = bind_crypto_generichash_update(state, in, inlen)
    end function crypto_generichash_update

    function crypto_generichash_final(state, out) result(res)
      integer(kind=c_int) res
      type(crypto_generichash_blake2b_state) state
      character(len=*) out
      integer(kind=c_size_t) outlen
      outlen = len(out)
      res = bind_crypto_generichash_final(state, out, outlen)
    end function crypto_generichash_final

end module mod_crypto_generichash
