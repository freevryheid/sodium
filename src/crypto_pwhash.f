module mod_crypto_pwhash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_long_long, c_ptr
  use mod_common
  use mod_crypto_pwhash_argon2i
  use mod_crypto_pwhash_argon2id
  implicit none
  private

  public crypto_pwhash_alg_argon2i13
  public crypto_pwhash_alg_argon2id13
  public crypto_pwhash_alg_default
  public crypto_pwhash_bytes_min
  public crypto_pwhash_bytes_max
  public crypto_pwhash_passwd_min
  public crypto_pwhash_passwd_max
  public crypto_pwhash_saltbytes
  public crypto_pwhash_strbytes
  public crypto_pwhash_strprefix
  public crypto_pwhash_opslimit_min
  public crypto_pwhash_opslimit_max
  public crypto_pwhash_memlimit_min
  public crypto_pwhash_memlimit_max
  public crypto_pwhash_opslimit_interactive
  public crypto_pwhash_memlimit_interactive
  public crypto_pwhash_opslimit_moderate
  public crypto_pwhash_memlimit_moderate
  public crypto_pwhash_opslimit_sensitive
  public crypto_pwhash_memlimit_sensitive
  public crypto_pwhash
  public crypto_pwhash_str
  public crypto_pwhash_str_alg
  public crypto_pwhash_str_verify
  public crypto_pwhash_str_needs_rehash
  public crypto_pwhash_primitive

  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_ALG_ARGON2I13        = &
    SODIUM_crypto_pwhash_argon2i_ALG_ARGON2I13
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_ALG_ARGON2ID13       = &
    SODIUM_crypto_pwhash_argon2id_ALG_ARGON2ID13
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_ALG_DEFAULT          = &
    SODIUM_crypto_pwhash_ALG_ARGON2ID13
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_BYTES_MIN            = &
    SODIUM_crypto_pwhash_argon2id_BYTES_MIN
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_BYTES_MAX            = &
    SODIUM_crypto_pwhash_argon2id_BYTES_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_PASSWD_MIN           = &
    SODIUM_crypto_pwhash_argon2id_PASSWD_MIN
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_PASSWD_MAX           = &
    SODIUM_crypto_pwhash_argon2id_PASSWD_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_SALTBYTES            = &
    SODIUM_crypto_pwhash_argon2id_SALTBYTES
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_STRBYTES             = &
    SODIUM_crypto_pwhash_argon2id_STRBYTES
  character(len=*), parameter, public :: SODIUM_crypto_pwhash_STRPREFIX                  = &
    SODIUM_crypto_pwhash_argon2id_STRPREFIX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_OPSLIMIT_MIN         = &
    SODIUM_crypto_pwhash_argon2id_OPSLIMIT_MIN
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_OPSLIMIT_MAX         = &
    SODIUM_crypto_pwhash_argon2id_OPSLIMIT_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_MEMLIMIT_MIN         = &
    SODIUM_crypto_pwhash_argon2id_MEMLIMIT_MIN
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_MEMLIMIT_MAX         = &
    SODIUM_crypto_pwhash_argon2id_MEMLIMIT_MAX
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_OPSLIMIT_INTERACTIVE = &
    SODIUM_crypto_pwhash_argon2id_OPSLIMIT_INTERACTIVE
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_MEMLIMIT_INTERACTIVE = &
    SODIUM_crypto_pwhash_argon2id_MEMLIMIT_INTERACTIVE
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_OPSLIMIT_MODERATE    = &
    SODIUM_crypto_pwhash_argon2id_OPSLIMIT_MODERATE
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_MEMLIMIT_MODERATE    = &
    SODIUM_crypto_pwhash_argon2id_MEMLIMIT_MODERATE
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_OPSLIMIT_SENSITIVE   = &
    SODIUM_crypto_pwhash_argon2id_OPSLIMIT_SENSITIVE
  integer(kind=c_size_t), parameter, public :: SODIUM_crypto_pwhash_MEMLIMIT_SENSITIVE   = &
    SODIUM_crypto_pwhash_argon2id_MEMLIMIT_SENSITIVE
  character(len=*), parameter, public :: SODIUM_crypto_pwhash_PRIMITIVE                  = &
    "argon2i"

  interface

    function crypto_pwhash_alg_argon2i13() &
    bind(c, name='crypto_pwhash_alg_argon2i13') &
    result(res)
      import c_int
      integer(kind=c_int) res
    end function crypto_pwhash_alg_argon2i13

    function crypto_pwhash_alg_argon2id13() &
    bind(c, name='crypto_pwhash_alg_argon2id13') &
    result(res)
      import c_int
      integer(kind=c_int) res
    end function crypto_pwhash_alg_argon2id13

    function crypto_pwhash_alg_default() &
    bind(c, name='crypto_pwhash_alg_default') &
    result(res)
      import c_int
      integer(kind=c_int) res
    end function crypto_pwhash_alg_default

    function crypto_pwhash_bytes_min() &
    bind(c, name='crypto_pwhash_bytes_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_bytes_min

    function crypto_pwhash_bytes_max() &
    bind(c, name='crypto_pwhash_bytes_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_bytes_max

    function crypto_pwhash_passwd_min() &
    bind(c, name='crypto_pwhash_passwd_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_passwd_min

    function crypto_pwhash_passwd_max() &
    bind(c, name='crypto_pwhash_passwd_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_passwd_max

    function crypto_pwhash_saltbytes() &
    bind(c, name='crypto_pwhash_saltbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_saltbytes

    function crypto_pwhash_strbytes() &
    bind(c, name='crypto_pwhash_strbytes') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_strbytes

    function bind_crypto_pwhash_strprefix() &
    bind(c, name='crypto_pwhash_strprefix') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_pwhash_strprefix

    function crypto_pwhash_opslimit_min() &
    bind(c, name='crypto_pwhash_opslimit_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_opslimit_min

    function crypto_pwhash_opslimit_max() &
    bind(c, name='crypto_pwhash_opslimit_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_opslimit_max

    function crypto_pwhash_memlimit_min() &
    bind(c, name='crypto_pwhash_memlimit_min') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_memlimit_min

    function crypto_pwhash_memlimit_max() &
    bind(c, name='crypto_pwhash_memlimit_max') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_memlimit_max

    function crypto_pwhash_opslimit_interactive() &
    bind(c, name='crypto_pwhash_opslimit_interactive') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_opslimit_interactive

    function crypto_pwhash_memlimit_interactive() &
    bind(c, name='crypto_pwhash_memlimit_interactive') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_memlimit_interactive

    function crypto_pwhash_opslimit_moderate() &
    bind(c, name='crypto_pwhash_opslimit_moderate') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_opslimit_moderate

    function crypto_pwhash_memlimit_moderate() &
    bind(c, name='crypto_pwhash_memlimit_moderate') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_memlimit_moderate

    function crypto_pwhash_opslimit_sensitive() &
    bind(c, name='crypto_pwhash_opslimit_sensitive') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_opslimit_sensitive

    function crypto_pwhash_memlimit_sensitive() &
    bind(c, name='crypto_pwhash_memlimit_sensitive') &
    result(res)
      import c_size_t
      integer(kind=c_size_t) res
    end function crypto_pwhash_memlimit_sensitive

    function bind_crypto_pwhash(out, outlen, passwd, passwdlen, salt, opslimit, memlimit, alg) &
    bind(c, name='crypto_pwhash') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) passwd
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: outlen, passwdlen, opslimit
      character(kind=c_char) salt
      integer(kind=c_int), value :: alg
    end function bind_crypto_pwhash

    function bind_crypto_pwhash_str(out, passwd, passwdlen, opslimit, memlimit) &
    bind(c, name='crypto_pwhash_str') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) passwd
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: passwdlen, opslimit
    end function bind_crypto_pwhash_str

    function bind_crypto_pwhash_str_alg(out, passwd, passwdlen, opslimit, memlimit, alg) &
    bind(c, name='crypto_pwhash_str_alg') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) out
      character(kind=c_char) passwd
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: passwdlen, opslimit
      integer(kind=c_int), value :: alg
    end function bind_crypto_pwhash_str_alg

    function bind_crypto_pwhash_str_verify(str, passwd, passwdlen) &
    bind(c, name='crypto_pwhash_str_verify') &
    result(res)
      import c_char, c_int, c_long_long
      integer(kind=c_int) res
      character(kind=c_char) str
      character(kind=c_char) passwd
      integer(kind=c_long_long), value :: passwdlen
    end function bind_crypto_pwhash_str_verify

    function crypto_pwhash_str_needs_rehash(str, opslimit, memlimit) &
    bind(c, name='crypto_pwhash_str_needs_rehash') &
    result(res)
      import c_char, c_int, c_long_long, c_size_t
      integer(kind=c_int) res
      character(kind=c_char) str
      integer(kind=c_size_t), value :: memlimit
      integer(kind=c_long_long), value :: opslimit
    end function crypto_pwhash_str_needs_rehash

    function bind_crypto_pwhash_primitive() &
    bind(c, name='crypto_pwhash_primitive') &
    result(res)
      import c_ptr
      type(c_ptr) res
    end function bind_crypto_pwhash_primitive

  end interface

  contains

    function crypto_pwhash_strprefix() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_pwhash_strprefix()
      call c_f_str_ptr(res1, res)
    end function crypto_pwhash_strprefix

    function crypto_pwhash_primitive() result(res)
      type(c_ptr) res1
      character(len=:), allocatable :: res
      res1 = bind_crypto_pwhash_primitive()
      call c_f_str_ptr(res1, res)
    end function crypto_pwhash_primitive

    function crypto_pwhash(out, passwd, salt, opslimit, memlimit, alg) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) passwd
      character(len=*) salt
      integer(kind=c_long_long), optional :: opslimit
      integer(kind=c_long_long) opslimit1
      integer(kind=c_size_t), optional :: memlimit
      integer(kind=c_size_t) memlimit1
      integer(kind=c_int), optional :: alg
      integer(kind=c_int) alg1
      integer(kind=c_long_long) outlen, passwdlen
      outlen = len(out)
      passwdlen = len(passwd)
      if (.not.present(opslimit)) then
        opslimit1 = SODIUM_crypto_pwhash_OPSLIMIT_INTERACTIVE
      else
        opslimit1 = opslimit
      end if
      if (.not.present(memlimit)) then
        memlimit1 = SODIUM_crypto_pwhash_MEMLIMIT_INTERACTIVE
      else
        memlimit1 = memlimit
      end if
      if (.not.present(alg)) then
        alg1 = SODIUM_crypto_pwhash_ALG_DEFAULT
      else
        alg1 = alg
      end if
      res = bind_crypto_pwhash(out, outlen, passwd, passwdlen, salt, opslimit1, memlimit1, alg1)
    end function crypto_pwhash

    function crypto_pwhash_str(out, passwd, opslimit, memlimit) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) passwd
      integer(kind=c_long_long) passwdlen
      integer(kind=c_long_long), optional :: opslimit
      integer(kind=c_long_long) opslimit1
      integer(kind=c_size_t), optional :: memlimit
      integer(kind=c_size_t) memlimit1
      passwdlen = len(passwd)
      if (.not.present(opslimit)) then
        opslimit1 = SODIUM_crypto_pwhash_OPSLIMIT_INTERACTIVE
      else
        opslimit1 = opslimit
      end if
      if (.not.present(memlimit)) then
        memlimit1 = SODIUM_crypto_pwhash_MEMLIMIT_INTERACTIVE
      else
        memlimit1 = memlimit
      end if
      res = bind_crypto_pwhash_str(out, passwd, passwdlen, opslimit1, memlimit1)
    end function crypto_pwhash_str

    function crypto_pwhash_str_alg(out, passwd, opslimit, memlimit, alg) result(res)
      integer(kind=c_int) res
      character(len=*) out
      character(len=*) passwd
      integer(kind=c_long_long) passwdlen
      integer(kind=c_long_long), optional :: opslimit
      integer(kind=c_long_long) opslimit1
      integer(kind=c_size_t), optional :: memlimit
      integer(kind=c_size_t) memlimit1
      integer(kind=c_int), optional :: alg
      integer(kind=c_int) alg1
      passwdlen = len(passwd)
      if (.not.present(opslimit)) then
        opslimit1 = SODIUM_crypto_pwhash_OPSLIMIT_INTERACTIVE
      else
        opslimit1 = opslimit
      end if
      if (.not.present(memlimit)) then
        memlimit1 = SODIUM_crypto_pwhash_MEMLIMIT_INTERACTIVE
      else
        memlimit1 = memlimit
      end if
      if (.not.present(alg)) then
        alg1 = SODIUM_crypto_pwhash_ALG_DEFAULT
      else
        alg1 = alg
      end if
      res = bind_crypto_pwhash_str_alg(out, passwd, passwdlen, opslimit1, memlimit1, alg1)
    end function crypto_pwhash_str_alg

    function crypto_pwhash_str_verify(str, passwd) result(res)
      integer(kind=c_int) res
      character(len=*) str
      character(len=*) passwd
      integer(kind=c_long_long) passwdlen
      passwdlen = len(passwd)
      res = bind_crypto_pwhash_str_verify(str, passwd, passwdlen)
    end function crypto_pwhash_str_verify

end module mod_crypto_pwhash
