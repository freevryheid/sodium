program tests_crypto_auth
  use, intrinsic :: iso_c_binding, only : c_long_long, c_size_t
  use sodium
  implicit none

  block
    character(len=:), allocatable :: key, mac, msg
    integer(kind=c_size_t) :: klen, mlen, res
    integer(kind=c_long_long) :: msg_len
    klen = crypto_auth_keybytes()
    mlen = crypto_auth_bytes()
    msg = "test"
    msg_len = int(len(msg), kind=c_long_long)
    allocate (character(len=klen) :: key)
    allocate (character(len=mlen) :: mac)
    call crypto_auth_keygen(key)
    res = crypto_auth(mac, msg, msg_len, key)
    if (res.ne.0) &
      error stop "error: crypto_auth failed"
    res = crypto_auth_verify(mac, msg, msg_len, key)
    if (res.ne.0) &
      error stop "error: crypto_auth_verify failed"
    deallocate(msg)
    deallocate(key)
    deallocate(mac)
  end block

  block
    character(len=PARAM_crypto_auth_KEYBYTES) :: key
    character(len=PARAM_crypto_auth_BYTES) :: mac
    integer :: res
    call crypto_auth_keygen(key)
    res = crypto_auth(mac, "Hello, world!", 13_c_size_t, key)
    if (res.ne.0) &
      error stop "error: crypto_auth failed"
    res = crypto_auth_verify(mac, "Hello, world!", 13_c_size_t, key)
    if (res.ne.0) &
      error stop "error: crypto_auth_verify failed"
  end block

  block
    if (crypto_auth_bytes().ne.PARAM_crypto_auth_BYTES) &
      error stop "error: crypto_auth_bytes failed"
    if (crypto_auth_keybytes().ne.PARAM_crypto_auth_KEYBYTES) &
      error stop "error: crypto_auth_keybytes failed"
    if (crypto_auth_primitive().ne.PARAM_crypto_auth_PRIMITIVE) &
      error stop "error: crypto_auth_primitive failed"
  end block

end program tests_crypto_auth