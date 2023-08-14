module mod_crypto_core_salsa20
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char
  implicit none
  private

  public crypto_core_salsa20_outputbytes
  public crypto_core_salsa20_inputbytes
  public crypto_core_salsa20_keybytes
  public crypto_core_salsa20_constbytes
  public crypto_core_salsa20

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa20_OUTPUTBYTES = 64
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa20_INPUTBYTES  = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa20_KEYBYTES    = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa20_CONSTBYTES  = 16

  interface

    function crypto_core_salsa20_outputbytes() &
    bind(c, name='crypto_core_salsa20_outputbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa20_outputbytes

    function crypto_core_salsa20_inputbytes() &
    bind(c, name='crypto_core_salsa20_inputbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa20_inputbytes

    function crypto_core_salsa20_keybytes() &
    bind(c, name='crypto_core_salsa20_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa20_keybytes

    function crypto_core_salsa20_constbytes() &
    bind(c, name='crypto_core_salsa20_constbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa20_constbytes

    function crypto_core_salsa20(out, in, k, c) &
    bind(c, name='crypto_core_salsa20') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) out, in, k, c
    end function crypto_core_salsa20

  end interface

end module mod_crypto_core_salsa20
