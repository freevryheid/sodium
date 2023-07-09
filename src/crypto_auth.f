module mod_crypto_auth

  use, intrinsic :: iso_c_binding, only : c_size_t, c_ptr, c_long_long, c_char, c_int
  use :: mod_common

  implicit none

  private

  public :: crypto_auth_bytes
  public :: crypto_auth_keybytes
  public :: crypto_auth_primitive
  public :: crypto_auth
  public :: crypto_auth_verify
  public :: crypto_auth_keygen

  interface

    function crypto_auth_bytes() &
    bind(c, name='crypto_auth_bytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_auth_bytes

    function crypto_auth_keybytes() &
    bind(c, name='crypto_auth_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_auth_keybytes

    function bind_crypto_auth_primitive() &
    bind(c, name='crypto_auth_primitive') &
    result(res)
      import :: c_ptr
      type(c_ptr) :: res
    end function bind_crypto_auth_primitive

    function crypto_auth(out, in, inlen, k) &
    bind(c, name='crypto_auth') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) :: k
    end function crypto_auth

    function crypto_auth_verify(h, in, inlen, k) &
    bind(c, name='crypto_auth_verify') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: h
      character(kind=c_char) :: in
      integer(kind=c_long_long), value :: inlen
      character(kind=c_char) :: k
    end function crypto_auth_verify

    subroutine crypto_auth_keygen(k) &
    bind(c, name='crypto_auth_keygen')
      import :: c_char
      character(kind=c_char) :: k
    end subroutine crypto_auth_keygen

  end interface

  contains

    function crypto_auth_primitive() result(res)
      character(len=:), allocatable :: res
      type(c_ptr) :: cptr
      cptr = bind_crypto_auth_primitive()
      call c_f_str_ptr(cptr, res)
    end function crypto_auth_primitive

end module mod_crypto_auth
