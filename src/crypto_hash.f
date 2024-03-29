module mod_crypto_hash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr
  use mod_common, only : c_f_str_ptr
  use mod_crypto_hash_sha512, only : SODIUM_crypto_hash_sha512_BYTES
  implicit none
  private

  public crypto_hash_bytes
  public crypto_hash
  public crypto_hash_primitive

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_hash_BYTES = SODIUM_crypto_hash_sha512_BYTES
  character(len=*), parameter, public :: SODIUM_crypto_hash_PRIMITIVE = "sha512"

  interface

    function crypto_hash_bytes() &
    bind(c, name='crypto_hash_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_hash_bytes

    function crypto_hash(out, in, inlen) &
    bind(c, name='crypto_hash') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) in
      integer(kind=c_long_long), value :: inlen
    end function crypto_hash

    function bind_crypto_hash_primitive() &
    bind(c, name='crypto_hash_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_hash_primitive

  end interface

contains

  function crypto_hash_primitive() &
  result(res)
    type(c_ptr) res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_hash_primitive()
    call c_f_str_ptr(res1, res)
  end function crypto_hash_primitive

end module mod_crypto_hash
