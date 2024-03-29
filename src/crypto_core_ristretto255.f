module mod_crypto_core_ristretto255
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char
  implicit none
  private

  public crypto_core_ristretto255_bytes
  public crypto_core_ristretto255_hashbytes
  public crypto_core_ristretto255_scalarbytes
  public crypto_core_ristretto255_nonreducedscalarbytes
  public crypto_core_ristretto255_is_valid_point
  public crypto_core_ristretto255_add
  public crypto_core_ristretto255_sub
  public crypto_core_ristretto255_from_hash
  public crypto_core_ristretto255_random
  public crypto_core_ristretto255_scalar_random
  public crypto_core_ristretto255_scalar_invert
  public crypto_core_ristretto255_scalar_negate
  public crypto_core_ristretto255_scalar_complement
  public crypto_core_ristretto255_scalar_add
  public crypto_core_ristretto255_scalar_sub
  public crypto_core_ristretto255_scalar_mul
  public crypto_core_ristretto255_scalar_reduce

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_BYTES                 = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_HASHBYTES             = 64
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_SCALARBYTES           = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_NONREDUCEDSCALARBYTES = 64
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_H2CSHA256             = 1
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_ristretto255_H2CSHA512             = 2

  interface

    function crypto_core_ristretto255_bytes() &
    bind(c, name='crypto_core_ristretto255_bytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_ristretto255_bytes

    function crypto_core_ristretto255_hashbytes() &
    bind(c, name='crypto_core_ristretto255_hashbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_ristretto255_hashbytes

    function crypto_core_ristretto255_scalarbytes() &
    bind(c, name='crypto_core_ristretto255_scalarbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_ristretto255_scalarbytes

    function crypto_core_ristretto255_nonreducedscalarbytes() &
    bind(c, name='crypto_core_ristretto255_nonreducedscalarbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_ristretto255_nonreducedscalarbytes

    function crypto_core_ristretto255_is_valid_point(p) &
    bind(c, name='crypto_core_ristretto255_is_valid_point') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) p
    end function crypto_core_ristretto255_is_valid_point

    function crypto_core_ristretto255_add(r, p, q) &
    bind(c, name='crypto_core_ristretto255_add') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) r, p, q
    end function crypto_core_ristretto255_add

    function crypto_core_ristretto255_sub(r, p, q) &
    bind(c, name='crypto_core_ristretto255_sub') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) r, p, q
    end function crypto_core_ristretto255_sub

    function crypto_core_ristretto255_from_hash(p, r) &
    bind(c, name='crypto_core_ristretto255_from_hash') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) r, p
    end function crypto_core_ristretto255_from_hash

    subroutine crypto_core_ristretto255_random(p) &
    bind(c, name='crypto_core_ristretto255_random')
      import c_char
      character(kind=c_char) p
    end subroutine crypto_core_ristretto255_random

    subroutine crypto_core_ristretto255_scalar_random(r) &
    bind(c, name='crypto_core_ristretto255_scalar_random')
      import c_char
      character(kind=c_char) r
    end subroutine crypto_core_ristretto255_scalar_random

    function crypto_core_ristretto255_scalar_invert(recip, s) &
    bind(c, name='crypto_core_ristretto255_scalar_invert') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) recip, s
    end function crypto_core_ristretto255_scalar_invert

    subroutine crypto_core_ristretto255_scalar_negate(neg, s) &
    bind(c, name='crypto_core_ristretto255_scalar_negate')
      import c_char
      character(kind=c_char) neg, s
    end subroutine crypto_core_ristretto255_scalar_negate

    subroutine crypto_core_ristretto255_scalar_complement(comp, s) &
    bind(c, name='crypto_core_ristretto255_scalar_complement')
      import c_char
      character(kind=c_char) comp, s
    end subroutine crypto_core_ristretto255_scalar_complement

    subroutine crypto_core_ristretto255_scalar_add(z, x, y) &
    bind(c, name='crypto_core_ristretto255_scalar_add')
      import c_char
      character(kind=c_char) z, x, y
    end subroutine crypto_core_ristretto255_scalar_add

    subroutine crypto_core_ristretto255_scalar_sub(z, x, y) &
    bind(c, name='crypto_core_ristretto255_scalar_sub')
      import c_char
      character(kind=c_char) z, x, y
    end subroutine crypto_core_ristretto255_scalar_sub

    subroutine crypto_core_ristretto255_scalar_mul(z, x, y) &
    bind(c, name='crypto_core_ristretto255_scalar_mul')
      import c_char
      character(kind=c_char) z, x, y
    end subroutine crypto_core_ristretto255_scalar_mul

    subroutine crypto_core_ristretto255_scalar_reduce(r, s) &
    bind(c, name='crypto_core_ristretto255_scalar_reduce')
      import c_char
      character(kind=c_char) r, s
    end subroutine crypto_core_ristretto255_scalar_reduce

  end interface

end module mod_crypto_core_ristretto255
