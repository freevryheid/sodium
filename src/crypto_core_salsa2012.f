module mod_crypto_core_salsa2012
  use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_char
  implicit none
  private

  public crypto_core_salsa2012_outputbytes
  public crypto_core_salsa2012_inputbytes
  public crypto_core_salsa2012_keybytes
  public crypto_core_salsa2012_constbytes
  public crypto_core_salsa2012

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa2012_OUTPUTBYTES = 64
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa2012_INPUTBYTES  = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa2012_KEYBYTES    = 32
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_core_salsa2012_CONSTBYTES  = 16

  interface

    function crypto_core_salsa2012_outputbytes() &
    bind(c, name='crypto_core_salsa2012_outputbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa2012_outputbytes

    function crypto_core_salsa2012_inputbytes() &
    bind(c, name='crypto_core_salsa2012_inputbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa2012_inputbytes

    function crypto_core_salsa2012_keybytes() &
    bind(c, name='crypto_core_salsa2012_keybytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa2012_keybytes

    function crypto_core_salsa2012_constbytes() &
    bind(c, name='crypto_core_salsa2012_constbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_core_salsa2012_constbytes

    function crypto_core_salsa2012(out, in, k, c) &
    bind(c, name='crypto_core_salsa2012') &
    result(res)
      import c_int, c_char
      integer(kind=c_int) res
      character(kind=c_char) out, in, k, c
    end function crypto_core_salsa2012

  end interface

end module mod_crypto_core_salsa2012
