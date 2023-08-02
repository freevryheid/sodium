module mod_crypto_core_salsa208
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char
  implicit none
  private

  public :: crypto_core_salsa208_outputbytes
  public :: crypto_core_salsa208_inputbytes
  public :: crypto_core_salsa208_keybytes
  public :: crypto_core_salsa208_constbytes
  public :: crypto_core_salsa208

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa208_OUTPUTBYTES = 64
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa208_INPUTBYTES  = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa208_KEYBYTES    = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa208_CONSTBYTES  = 16

  interface

    function crypto_core_salsa208_outputbytes() &
    bind(c, name='crypto_core_salsa208_outputbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_core_salsa208_outputbytes

    function crypto_core_salsa208_inputbytes() &
    bind(c, name='crypto_core_salsa208_inputbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_core_salsa208_inputbytes

    function crypto_core_salsa208_keybytes() &
    bind(c, name='crypto_core_salsa208_keybytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_core_salsa208_keybytes

    function crypto_core_salsa208_constbytes() &
    bind(c, name='crypto_core_salsa208_constbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_core_salsa208_constbytes

    function crypto_core_salsa208(out, in, k, c) &
    bind(c, name='crypto_core_salsa208') &
    result(res)
      import :: c_int, c_char
      integer(kind=c_int) :: res
      character(kind=c_char) :: out, in, k, c
    end function crypto_core_salsa208

  end interface

end module mod_crypto_core_salsa208