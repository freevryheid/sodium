module mod_crypto_shorthash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr
  use mod_common
  use  mod_crypto_shorthash_siphash24
  implicit none
  private

  public crypto_shorthash_bytes
  public crypto_shorthash_keybytes
  public crypto_shorthash_primitive
  public crypto_shorthash
  public crypto_shorthash_keygen

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_BYTES    = SODIUM_crypto_shorthash_siphash24_BYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_shorthash_KEYBYTES = SODIUM_crypto_shorthash_siphash24_KEYBYTES
  character(len=*), parameter, public :: SODIUM_crypto_shorthash_PRIMITIVE      = "siphash24"

  interface

    function crypto_shorthash_bytes() &
    bind(c, name='crypto_shorthash_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_shorthash_bytes

    function crypto_shorthash_keybytes() &
    bind(c, name='crypto_shorthash_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_shorthash_keybytes

    function bind_crypto_shorthash_primitive() &
    bind(c, name='crypto_shorthash_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_shorthash_primitive

    function bind_crypto_shorthash(out, in, inlen, k) &
    bind(c, name='crypto_shorthash') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) k
    end function bind_crypto_shorthash

    subroutine crypto_shorthash_keygen(k) &
    bind(c, name='crypto_shorthash_keygen')
      import c_char
      character(kind=c_char) k
    end subroutine crypto_shorthash_keygen

  end interface

  contains

    function crypto_shorthash_primitive() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_shorthash_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_shorthash_primitive

    function crypto_shorthash(out, in, k) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) in
      integer(kind=c_long_long) inlen
      character(len=*) k
      inlen = len(in)
      res = bind_crypto_shorthash(out, in, inlen, k)
    end function crypto_shorthash

end module mod_crypto_shorthash
