module mod_crypto_hash_sha512
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_int8_t, c_int64_t
  implicit none
  private

  public :: crypto_hash_sha512_statebytes
  public :: crypto_hash_sha512_bytes
  public :: crypto_hash_sha512
  public :: crypto_hash_sha512_init
  public :: crypto_hash_sha512_update
  public :: crypto_hash_sha512_final

  integer, parameter, public :: SODIUM_crypto_hash_sha512_BYTES = 64

  ! TODO: perhaps bump these up to match unsigned equivalents
  type, public, bind(c) :: crypto_hash_sha512_state
    integer(kind=c_int64_t) :: state(8)
    integer(kind=c_int64_t) :: count(2)
    integer(kind=c_int8_t) :: buf(128)
  end type

  interface

    function crypto_hash_sha512_statebytes() &
    bind(c, name='crypto_hash_sha512_statebytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_hash_sha512_statebytes

    function crypto_hash_sha512_bytes() &
    bind(c, name='crypto_hash_sha512_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_hash_sha512_bytes

    function crypto_hash_sha512(out, in, inlen) &
    bind(c, name='crypto_hash_sha512') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
    end function crypto_hash_sha512

    function crypto_hash_sha512_init(state) &
    bind(c, name='crypto_hash_sha512_init') &
    result(res)
      import :: c_int, crypto_hash_sha512_state
      integer(kind=c_int) :: res
      type(crypto_hash_sha512_state) :: state
    end function crypto_hash_sha512_init

    function crypto_hash_sha512_update(state, in, inlen) &
    bind(c, name='crypto_hash_sha512_update') &
    result(res)
      import :: c_int, crypto_hash_sha512_state, c_char, c_long_long
      integer(kind=c_int) :: res
      type(crypto_hash_sha512_state) :: state
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
    end function crypto_hash_sha512_update

    function crypto_hash_sha512_final(state, out) &
    bind(c, name='crypto_hash_sha512_final') &
    result(res)
      import :: c_int, crypto_hash_sha512_state, c_char
      integer(kind=c_int) :: res
      type(crypto_hash_sha512_state) :: state
      character(kind=c_char) :: out
    end function crypto_hash_sha512_final

  end interface

end module mod_crypto_hash_sha512
