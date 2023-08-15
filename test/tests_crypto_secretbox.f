program tests_crypto_secretbox
  use, intrinsic :: iso_c_binding, only : c_long_long
  use sodium
  implicit none
  block
    character(len=SODIUM_crypto_secretbox_KEYBYTES) key
    character(len=SODIUM_crypto_secretbox_NONCEBYTES) nonce
    character(len=:), allocatable :: msg, cipher, decrypted
    integer(kind=c_long_long) ml, cl
    integer ret
    call crypto_secretbox_keygen(key)
    call randombytes_buf(nonce)
    msg = "test"
    ml = int(len(msg), kind=c_long_long)
    cl = ml + SODIUM_crypto_secretbox_MACBYTES
    allocate (character(len=cl)::cipher)
    allocate (character(len=ml)::decrypted)
    ret = crypto_secretbox_easy(cipher, c_str(msg), ml, nonce, key)
    if (ret.ne.0) &
      error stop "error: crypto_secretbox_easy failed"
    ret = crypto_secretbox_open_easy(decrypted, cipher, cl, nonce, key)
    if (ret.ne.0) &
      error stop "error: crypto_secretbox_open_easy failed"
  end block
end program tests_crypto_secretbox
