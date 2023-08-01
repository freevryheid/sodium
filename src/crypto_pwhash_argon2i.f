module mod_crypto_pwhash_argon2i
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr
  use :: mod_common
  implicit none
  private

  public :: crypto_pwhash_argon2i_alg_argon2i13
  public :: crypto_pwhash_argon2i_bytes_min
  public :: crypto_pwhash_argon2i_bytes_max
  public :: crypto_pwhash_argon2i_passwd_min
  public :: crypto_pwhash_argon2i_passwd_max
  public :: crypto_pwhash_argon2i_saltbytes
  public :: crypto_pwhash_argon2i_strbytes
  public :: crypto_pwhash_argon2i_strprefix
  public :: crypto_pwhash_argon2i_opslimit_min
  public :: crypto_pwhash_argon2i_opslimit_max
  public :: crypto_pwhash_argon2i_memlimit_min
  public :: crypto_pwhash_argon2i_memlimit_max
  public :: crypto_pwhash_argon2i_opslimit_interactive
  public :: crypto_pwhash_argon2i_memlimit_interactive
  public :: crypto_pwhash_argon2i_opslimit_moderate
  public :: crypto_pwhash_argon2i_memlimit_moderate
  public :: crypto_pwhash_argon2i_opslimit_sensitive
  public :: crypto_pwhash_argon2i_memlimit_sensitive
  public :: crypto_pwhash_argon2i
  public :: crypto_pwhash_argon2i_str
  public :: crypto_pwhash_argon2i_str_verify
  public :: crypto_pwhash_argon2i_str_needs_rehash

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_ALG_ARGON2I13        = 1
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_BYTES_MIN            = 16
  ! integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_BYTES_MAX            = 4294967295
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_BYTES_MAX            = int(z'FFFFFFFF')
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_PASSWD_MIN           = 0
  ! integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_PASSWD_MAX           = 4294967295
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_PASSWD_MAX           = int(z'FFFFFFFF')
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_SALTBYTES            = 16
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_STRBYTES             = 128
  character(len=*), parameter, public :: SODIUM_crypto_pwhash_argon2i_STRPREFIX                  = "$argon2i$"
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_MIN         = 3
  ! integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_MAX         = 4294967295
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_MAX         = int(z'FFFFFFFF')
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_MIN         = 8192
  ! integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_MAX         = 4398046510080 ! TODO
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_MAX         = int(z'3FFFFFFFC00')
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_INTERACTIVE = 4
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_INTERACTIVE = 33554432
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_MODERATE    = 6
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_MODERATE    = 134217728
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_OPSLIMIT_SENSITIVE   = 8
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_argon2i_MEMLIMIT_SENSITIVE   = 536870912

  interface

    function crypto_pwhash_argon2i_alg_argon2i13() &
    bind(c, name='crypto_pwhash_argon2i_alg_argon2i13') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function crypto_pwhash_argon2i_alg_argon2i13

    function crypto_pwhash_argon2i_bytes_min() &
    bind(c, name='crypto_pwhash_argon2i_bytes_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_bytes_min

    function crypto_pwhash_argon2i_bytes_max() &
    bind(c, name='crypto_pwhash_argon2i_bytes_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_bytes_max

    function crypto_pwhash_argon2i_passwd_min() &
    bind(c, name='crypto_pwhash_argon2i_passwd_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_passwd_min

    function crypto_pwhash_argon2i_passwd_max() &
    bind(c, name='crypto_pwhash_argon2i_passwd_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_passwd_max

    function crypto_pwhash_argon2i_saltbytes() &
    bind(c, name='crypto_pwhash_argon2i_saltbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_saltbytes

    function crypto_pwhash_argon2i_strbytes() &
    bind(c, name='crypto_pwhash_argon2i_strbytes') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_strbytes

    function bind_crypto_pwhash_argon2i_strprefix() &
    bind(c, name='crypto_pwhash_argon2i_strprefix') &
    result(res)
      import :: c_ptr
      type(c_ptr) :: res
    end function bind_crypto_pwhash_argon2i_strprefix

    function crypto_pwhash_argon2i_opslimit_min() &
    bind(c, name='crypto_pwhash_argon2i_opslimit_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_opslimit_min

    function crypto_pwhash_argon2i_opslimit_max() &
    bind(c, name='crypto_pwhash_argon2i_opslimit_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_opslimit_max

    function crypto_pwhash_argon2i_memlimit_min() &
    bind(c, name='crypto_pwhash_argon2i_memlimit_min') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_memlimit_min

    function crypto_pwhash_argon2i_memlimit_max() &
    bind(c, name='crypto_pwhash_argon2i_memlimit_max') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_memlimit_max

    function crypto_pwhash_argon2i_opslimit_interactive() &
    bind(c, name='crypto_pwhash_argon2i_opslimit_interactive') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_opslimit_interactive

    function crypto_pwhash_argon2i_memlimit_interactive() &
    bind(c, name='crypto_pwhash_argon2i_memlimit_interactive') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_memlimit_interactive

    function crypto_pwhash_argon2i_opslimit_moderate() &
    bind(c, name='crypto_pwhash_argon2i_opslimit_moderate') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_opslimit_moderate

    function crypto_pwhash_argon2i_memlimit_moderate() &
    bind(c, name='crypto_pwhash_argon2i_memlimit_moderate') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_memlimit_moderate

    function crypto_pwhash_argon2i_opslimit_sensitive() &
    bind(c, name='crypto_pwhash_argon2i_opslimit_sensitive') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_opslimit_sensitive

    function crypto_pwhash_argon2i_memlimit_sensitive() &
    bind(c, name='crypto_pwhash_argon2i_memlimit_sensitive') &
    result(res)
      import :: c_size_t
      integer(kind=c_size_t) :: res
    end function crypto_pwhash_argon2i_memlimit_sensitive

    function crypto_pwhash_argon2i(out, outlen, passwd, passwdlen, salt, opslimit, memlimit, alg) &
    bind(c, name='crypto_pwhash_argon2i') &
    result(res)
      import :: c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: passwd
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: outlen, passwdlen, opslimit
      character(kind=c_char) :: salt
      integer(kind=c_int), value :: alg
    end function crypto_pwhash_argon2i

    function crypto_pwhash_argon2i_str(out, passwd, passwdlen, opslimit, memlimit) &
    bind(c, name='crypto_pwhash_argon2i_str') &
    result(res)
      import :: c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: out
      character(kind=c_char) :: passwd
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: passwdlen, opslimit
    end function crypto_pwhash_argon2i_str

    function crypto_pwhash_argon2i_str_verify(str, passwd, passwdlen) &
    bind(c, name='crypto_pwhash_argon2i_str_verify') &
    result(res)
      import :: c_char, c_int, c_long_long
      integer(kind=c_int) :: res
      character(kind=c_char) :: str
      character(kind=c_char) :: passwd
      integer(kind=c_long_long), value :: passwdlen
    end function crypto_pwhash_argon2i_str_verify

    function crypto_pwhash_argon2i_str_needs_rehash(str, opslimit, memlimit) &
    bind(c, name='crypto_pwhash_argon2i_str_needs_rehash') &
    result(res)
      import :: c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: str
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: opslimit
    end function crypto_pwhash_argon2i_str_needs_rehash

  end interface

contains

  function crypto_pwhash_argon2i_strprefix() result(res)
    type(c_ptr) :: res1
    character(len=:), allocatable :: res
    res1 = bind_crypto_pwhash_argon2i_strprefix()
    call c_f_str_ptr(res1, res)
  end function crypto_pwhash_argon2i_strprefix

end module mod_crypto_pwhash_argon2i
