program tests_utils

  use, intrinsic :: iso_c_binding!, only : c_size_t
  use sodium
  implicit none

  if (sodium_init().ne.0) &
    error stop "error: sodium init failed"

  block
    character(len=:), allocatable :: hex
    hex = sodium_bin2hex("abcdefghij")
    if (hex.ne."6162636465666768696a") &
      error stop "error: sodium_bin2hex failed"
    deallocate(hex)
  end block

  block
    character(len=:), allocatable :: bin
    bin = sodium_hex2bin("6162636465666768696a")
    if (bin.ne."abcdefghij") &
      error stop "error: sodium_hex2bin failed"
    deallocate(bin)
  end block

  block
    character(len=:), allocatable :: b64
    b64 = sodium_bin2base64("11")
    if (b64.ne."MTE=") &
      error stop "error: sodium_bin2base64 failed"
    deallocate(b64)
  end block

  block
    character(len=:), allocatable :: bin
    bin = sodium_base642bin("MTE=")
    if (bin.ne."11") &
      error stop "error: sodium_base642bin failed"
    deallocate(bin)
  end block

  block
    character(len=4) a
    integer(kind=4) i
    i = 12345
    a = transfer(i, a)
    call sodium_increment(a)
    i = transfer(a, i)
    if (i.ne.12346) &
      error stop "error: sodium_increment failed"
  end block

  block
    character(len=4) a, b
    integer(kind=4) i
    i = 12345
    a = transfer(i, a)
    b = a
    call sodium_add(a, b)
    i = transfer(a, i)
    if (i.ne.24690) &
      error stop "error: sodium_add failed"
  end block

  block
    character(len=4) a, b
    integer(kind=4) i
    i = 12345
    a = transfer(i, a)
    b = a
    call sodium_sub(a, b)
    i = transfer(a, i)
    if (i.ne.0) &
      error stop "error: sodium_sub failed"
  end block

  block
    integer r
    character(len=4) a, b
    integer(kind=4) i, j
    i = 12345
    a = transfer(i, a)
    b = a
    r = sodium_compare(a, b)
    if (r.ne.0) &
      error stop "error: sodium_compare failed"
    j = i - 1
    b = transfer(j, b)
    r = sodium_compare(a, b)
    if (r.ne.1) &
      error stop "error: sodium_compare failed"
    j = i + 1
    b = transfer(j, b)
    r = sodium_compare(a, b)
    if (r.ne.-1) &
      error stop "error: sodium_compare failed"
  end block

  block
    integer r
    character(len=4) a
    integer(kind=4) i
    i = 0
    a = transfer(i, a)
    r = sodium_is_zero(a)
    if (r.ne.1) &
      error stop "error: sodium_is_zero failed"
    i = 1
    a = transfer(i, a)
    r = sodium_is_zero(a)
    if (r.ne.0) &
      error stop "error: sodium_is_zero failed"
  end block

  block
    integer(kind=c_size_t) :: buflen, padded_buflen, blocksize, max_buflen
    integer(kind=c_int) :: i, res
    ! character(len=max_buflen) :: buf
    character(len=20) :: buf
    blocksize = 4
    buflen = 16
    max_buflen = buflen + (blocksize - modulo(buflen, blocksize))
    buf = "                "
    res = sodium_pad(padded_buflen, buf, buflen, blocksize, max_buflen)
    if (res.ne.0) &
      error stop "error: sodium_pad failed"
    res = sodium_unpad(buflen, buf, padded_buflen, blocksize)
    if (res.ne.0) &
      error stop "error: sodium_unpad failed"
  end block

  block
    character(len=20) :: buf
    integer(kind=c_size_t) :: blen
    integer(kind=c_int) :: res
    blen = 20
    call sodium_memzero(buf, blen)
    res = sodium_is_zero(buf)
    if (res.ne.1) &
      error stop "error: sodium_memzero failed"
    buf = "XXXXXXXXXXXXXXXXXXXX"
    blen = 10
    call sodium_memzero(buf, blen)
    res = sodium_is_zero(buf, blen)
    if (res.ne.1) &
      error stop "error: sodium_memzero failed"
  end block

  block
    character(len=:), pointer :: res => null()
    res => sodium_malloc(64_c_size_t)
    call sodium_free(res)
  end block

end program tests_utils