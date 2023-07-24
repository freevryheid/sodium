module mod_utils
  use, intrinsic :: iso_c_binding, only : c_size_t, c_char, c_int, c_ptr, c_loc, &
    c_f_pointer, c_null_char, c_null_ptr
  use :: mod_common
  implicit none
  private

  public :: sodium_memzero
  public :: sodium_stackzero
  public :: sodium_memcmp
  public :: sodium_compare
  public :: sodium_is_zero
  public :: sodium_increment
  public :: sodium_add
  public :: sodium_sub
  public :: sodium_bin2hex
  public :: sodium_hex2bin
  public :: sodium_base64_encoded_len
  public :: sodium_bin2base64
  public :: sodium_base642bin
  public :: sodium_mlock
  public :: sodium_munlock
  public :: sodium_malloc
  public :: sodium_allocarray
  public :: sodium_free
  public :: sodium_mprotect_noaccess
  public :: sodium_mprotect_readonly
  public :: sodium_mprotect_readwrite
  public :: sodium_pad
  public :: sodium_unpad

  integer(kind=c_int), parameter, public :: PARAM_SODIUM_BASE64_VARIANT_ORIGINAL            = 1
  integer(kind=c_int), parameter, public :: PARAM_SODIUM_BASE64_VARIANT_ORIGINAL_NO_PADDING = 3
  integer(kind=c_int), parameter, public :: PARAM_SODIUM_BASE64_VARIANT_URLSAFE             = 5
  integer(kind=c_int), parameter, public :: PARAM_SODIUM_BASE64_VARIANT_URLSAFE_NO_PADDING  = 7

  interface

    subroutine bind_sodium_memzero(pnt, len) &
    bind(c, name='sodium_memzero')
      import :: c_char, c_size_t
      character(kind=c_char) :: pnt
      integer(kind=c_size_t), value :: len
    end subroutine bind_sodium_memzero

    subroutine sodium_stackzero(len) &
    bind(c, name='sodium_stackzero')
      import :: c_size_t
      integer(kind=c_size_t), value :: len
    end subroutine sodium_stackzero

    function sodium_memcmp(b1_, b2_, len) &
    bind(c, name='sodium_memcmp') &
    result(res)
      import :: c_int, c_size_t, c_char
      integer(kind=c_int) :: res
      character(kind=c_char) :: b1_
      character(kind=c_char) :: b2_
      integer(kind=c_size_t) :: len ! CHK:value
    end function sodium_memcmp

    function bind_sodium_compare(b1_, b2_, len) &
    bind(c, name='sodium_compare') &
    result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: b1_
      character(kind=c_char) :: b2_
      integer(kind=c_size_t), value :: len
    end function bind_sodium_compare

    function bind_sodium_is_zero(n, nlen) &
    bind(c, name='sodium_is_zero') &
    result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: n
      integer(kind=c_size_t), value :: nlen
    end function bind_sodium_is_zero

    subroutine bind_sodium_increment(n, nlen) &
    bind(c, name='sodium_increment')
      import :: c_char, c_size_t
      character(kind=c_char) :: n
      integer(kind=c_size_t), value :: nlen
    end subroutine bind_sodium_increment

    subroutine bind_sodium_add(a, b, len) &
    bind(c, name='sodium_add')
      import :: c_char, c_size_t
      character(kind=c_char) :: a
      character(kind=c_char) :: b
      integer(kind=c_size_t), value :: len
    end subroutine bind_sodium_add

    subroutine bind_sodium_sub(a, b, len) &
    bind(c, name='sodium_sub')
      import :: c_char, c_size_t
      character(kind=c_char) :: a
      character(kind=c_char) :: b
      integer(kind=c_size_t), value :: len
    end subroutine bind_sodium_sub

    function bind_sodium_bin2hex(hex, hex_maxlen, bin, bin_len) &
    bind(c, name='sodium_bin2hex') &
    result(res)
      import :: c_char, c_ptr, c_size_t
      type(c_ptr) :: res
      character(kind=c_char) :: hex
      integer(kind=c_size_t), value :: hex_maxlen
      character(kind=c_char) :: bin
      integer(kind=c_size_t), value :: bin_len
    end function bind_sodium_bin2hex

    function bind_sodium_hex2bin(bin, bin_maxlen, hex, hex_len, ignore, bin_len, hex_end) &
    bind(c, name='sodium_hex2bin') &
    result(res)
      import :: c_char, c_int, c_ptr, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: bin
      integer(kind=c_size_t), value :: bin_maxlen
      character(kind=c_char) :: hex
      integer(kind=c_size_t), value :: hex_len
      character(kind=c_char) :: ignore
      integer(kind=c_size_t) :: bin_len
      type(c_ptr) :: hex_end
    end function bind_sodium_hex2bin

    function sodium_base64_encoded_len(bin_len, variant) &
    bind(c, name='sodium_base64_encoded_len') &
    result(res)
      import :: c_int, c_size_t
      integer(kind=c_size_t) :: res
      integer(kind=c_size_t), value :: bin_len
      integer(kind=c_int), value :: variant
    end function sodium_base64_encoded_len

    function bind_sodium_bin2base64(b64, b64_maxlen, bin, bin_len, variant) &
    bind(c, name='sodium_bin2base64') &
    result(res)
      import :: c_char, c_int, c_ptr, c_size_t
      type(c_ptr) :: res
      character(kind=c_char) :: b64
      integer(kind=c_size_t), value :: b64_maxlen
      character(kind=c_char) :: bin
      integer(kind=c_size_t), value :: bin_len
      integer(kind=c_int), value :: variant
    end function bind_sodium_bin2base64

    function bind_sodium_base642bin(bin, bin_maxlen, b64, b64_len, ignore, bin_len, b64_end, variant) &
    bind(c, name='sodium_base642bin') &
    result(res)
      import :: c_char, c_int, c_ptr, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: bin
      integer(kind=c_size_t), value :: bin_maxlen
      character(kind=c_char) :: b64
      integer(kind=c_size_t), value :: b64_len
      character(kind=c_char) :: ignore
      integer(kind=c_size_t) :: bin_len
      type(c_ptr) :: b64_end
      integer(kind=c_int), value :: variant
    end function bind_sodium_base642bin

    function sodium_mlock(addr, len) &
    bind(c, name='sodium_mlock') &
    result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: addr
      integer(kind=c_size_t), value :: len
    end function sodium_mlock

    function sodium_munlock(addr, len) &
    bind(c, name='sodium_munlock') &
    result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      character(kind=c_char) :: addr
      integer(kind=c_size_t), value :: len
    end function sodium_munlock

    function bind_sodium_malloc(size) &
    bind(c, name='sodium_malloc') &
    result(res)
      import :: c_ptr, c_size_t
      integer(kind=c_size_t), value :: size
      ! type(c_ptr), target :: res
      type(c_ptr) :: res
    end function bind_sodium_malloc

    function sodium_allocarray(count, size) &
    bind(c, name='sodium_allocarray') &
    result(res)
      import :: c_size_t, c_ptr
      integer(kind=c_size_t) :: count
      integer(kind=c_size_t) :: size
      type(c_ptr) :: res
    end function sodium_allocarray

    subroutine bind_sodium_free(ptr) &
    bind(c, name='sodium_free')
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine bind_sodium_free

    function bind_sodium_mprotect_noaccess(ptr) &
    bind(c, name='sodium_mprotect_noaccess') &
    result(res)
      import :: c_int, c_ptr
      integer(kind=c_int) :: res
      type(c_ptr), value :: ptr
    end function bind_sodium_mprotect_noaccess

    function bind_sodium_mprotect_readonly(ptr) &
    bind(c, name='sodium_mprotect_readonly') &
    result(res)
      import :: c_int, c_ptr
      integer(kind=c_int) :: res
      type(c_ptr), value :: ptr
    end function bind_sodium_mprotect_readonly

    function bind_sodium_mprotect_readwrite(ptr) &
    bind(c, name='sodium_mprotect_readwrite') &
    result(res)
      import :: c_int, c_ptr
      integer(kind=c_int) :: res
      type(c_ptr), value :: ptr
    end function bind_sodium_mprotect_readwrite

    function bind_sodium_pad(padded_buflen_p, buf, unpadded_buflen, blocksize, max_buflen) &
    bind(c, name='sodium_pad') result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      integer(kind=c_size_t) :: padded_buflen_p
      character(kind=c_char) :: buf
      integer(kind=c_size_t), value :: unpadded_buflen
      integer(kind=c_size_t), value :: blocksize
      integer(kind=c_size_t), value :: max_buflen
    end function bind_sodium_pad

    function bind_sodium_unpad(unpadded_buflen_p, buf, padded_buflen, blocksize) &
    bind(c, name='sodium_unpad') result(res)
      import :: c_char, c_int, c_size_t
      integer(kind=c_int) :: res
      integer(kind=c_size_t) :: unpadded_buflen_p
      character(kind=c_char) :: buf
      integer(kind=c_size_t), value :: padded_buflen
      integer(kind=c_size_t), value :: blocksize
    end function bind_sodium_unpad

  end interface

  contains

    function sodium_bin2hex(bin) result(res)
      character(len=*) :: bin
      character(len=:), allocatable :: hex
      character(len=:), allocatable :: res
      integer(kind=c_size_t) :: hex_maxlen
      integer(kind=c_size_t) :: bin_len
      type(c_ptr) :: res1
      bin_len = len(bin)
      hex_maxlen = bin_len*2 + 1
      allocate (character(len=hex_maxlen) :: hex)
      res1 = bind_sodium_bin2hex(hex, hex_maxlen, bin, bin_len)
      call c_f_str_ptr(res1, res)
    end function sodium_bin2hex

    function sodium_hex2bin(hex) result(bin)
      character(len=*) :: hex
      character(len=:), allocatable :: bin
      character(len=:), allocatable :: ignore
      type(c_ptr) :: hex_end
      integer(kind=c_size_t) :: hexlen
      integer(kind=c_size_t) :: max_binlen
      integer(kind=c_size_t) :: binlen
      integer(kind=c_size_t) :: tmp
      ignore = c_null_char ! disallow any non-hexadecimal character
      hex_end = c_null_ptr
      hexlen = len(hex)
      max_binlen = hexlen/2 + 1
      binlen = max_binlen
      allocate (character(len=binlen) :: bin)
      tmp = bind_sodium_hex2bin(bin, max_binlen, hex, hexlen, ignore, binlen, hex_end)
      if (tmp.eq.-1) then
        error stop "error: could not parse hex - see docs"
      end if
      bin = bin(1:binlen)
    end function sodium_hex2bin

    function sodium_bin2base64(bin) result(res)
      character(len=*) :: bin
      character(len=:), allocatable :: b64
      character(len=:), allocatable :: res
      integer(kind=c_size_t) :: max_b64len, binlen
      integer(kind=c_int) :: variant
      type(c_ptr) :: res1
      binlen = len(bin)
      variant = PARAM_SODIUM_BASE64_VARIANT_ORIGINAL
      max_b64len = sodium_base64_encoded_len(binlen, variant)
      allocate (character(len=max_b64len) :: b64)
      res1 = bind_sodium_bin2base64(b64, max_b64len, bin, binlen, variant)
      call c_f_str_ptr(res1, res)
    end function sodium_bin2base64

    function sodium_base642bin(b64) result(bin)
      character(len=*) :: b64
      character(len=:), allocatable :: bin
      character(len=:), allocatable :: ignore
      type(c_ptr) :: b64_end
      integer(kind=c_size_t) :: tmp
      integer(kind=c_size_t) :: b64len
      integer(kind=c_size_t) :: binlen
      integer(kind=c_size_t) :: max_binlen
      integer(kind=c_int) :: variant
      b64len = len(b64)
      ignore = c_null_char ! disallow any non-hexadecimal character
      b64_end = c_null_ptr
      variant = PARAM_SODIUM_BASE64_VARIANT_ORIGINAL
      max_binlen = b64len/4*3
      binlen = max_binlen
      allocate (character(len=binlen) :: bin)
      tmp = bind_sodium_base642bin(bin, max_binlen, b64, b64len, ignore, binlen, b64_end, variant)
      if (tmp.eq.-1) then
        error stop "error: could not parse base64 - see docs"
      end if
      bin = bin(1:binlen)
    end function sodium_base642bin

    subroutine sodium_increment(n) 
      character(len=*) ::  n
      integer(kind=c_size_t) :: nlen
      nlen = len(n)
      call bind_sodium_increment(n, nlen)
    end subroutine sodium_increment

    subroutine sodium_add(a, b) 
      character(len=*) ::  a, b
      integer(kind=c_size_t) :: alen
      alen = len(a)
      call bind_sodium_add(a, b, alen)
    end subroutine sodium_add

    subroutine sodium_sub(a, b) 
      character(len=*) ::  a, b
      integer(kind=c_size_t) :: alen
      alen = len(a)
      call bind_sodium_sub(a, b, alen)
    end subroutine sodium_sub

    function sodium_compare(a, b) result(res)
      character(len=*) ::  a, b
      integer(kind=c_size_t) :: alen
      integer :: res
      alen = len(a)
      res = int(bind_sodium_compare(a, b, alen))
    end function sodium_compare

    function sodium_is_zero(a, alen) result(res)
      character(len=*) ::  a
      integer(kind=c_size_t), optional :: alen
      integer(kind=c_size_t) :: blen 
      integer(kind=c_int) :: res
      if (.not.present(alen)) then
        blen = len(a)
      else
        blen = alen
      end if
      res = bind_sodium_is_zero(a, blen)
    end function sodium_is_zero

    function sodium_pad(padded_buflen_p, buf, unpadded_buflen, blocksize, max_buflen) result(res)
      character(len=*) :: buf
      integer(kind=c_int) :: res
      integer(kind=c_size_t) :: padded_buflen_p, unpadded_buflen, max_buflen, blocksize
      res = bind_sodium_pad( &
        padded_buflen_p, buf, &
        unpadded_buflen, &
        blocksize, &
        max_buflen)
    end function sodium_pad

    function sodium_unpad(unpadded_buflen_p, buf, padded_buflen, blocksize) result(res)
      character(len=*) :: buf
      integer(kind=c_int) :: res
      integer(kind=c_size_t) :: padded_buflen, unpadded_buflen_p, blocksize
      res = bind_sodium_unpad( &
        unpadded_buflen_p, buf, &
        padded_buflen, &
        blocksize)
    end function sodium_unpad

    subroutine sodium_memzero(pnt, len) 
      character(len=*) :: pnt
      integer(kind=c_size_t) :: len
      call bind_sodium_memzero(pnt, len)
    end subroutine sodium_memzero

    function sodium_malloc(siz) result(res)
      character(len=:), pointer:: res
      integer(kind=c_size_t) :: siz
      type(c_ptr) :: res1
      res1 = bind_sodium_malloc(siz)
      call c_f_pointer(res1, res)
      res => res(1:siz)
    end function sodium_malloc

    subroutine sodium_free(ptr)
      character(len=:), pointer :: ptr
      call bind_sodium_free(c_loc(ptr))
    end subroutine sodium_free

    function sodium_mprotect_noaccess(ptr) &
    result(res)
      integer(kind=c_int) :: res
      character(len=:), pointer :: ptr
      res = bind_sodium_mprotect_noaccess(c_loc(ptr))
    end function sodium_mprotect_noaccess

    function sodium_mprotect_readonly(ptr) &
    result(res)
      integer(kind=c_int) :: res
      character(len=:), pointer :: ptr
      res = bind_sodium_mprotect_readonly(c_loc(ptr))
    end function sodium_mprotect_readonly

    function sodium_mprotect_readwrite(ptr) &
    result(res)
      integer(kind=c_int) :: res
      character(len=:), pointer :: ptr
      res = bind_sodium_mprotect_readwrite(c_loc(ptr))
    end function sodium_mprotect_readwrite

end module mod_utils