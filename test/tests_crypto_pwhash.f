program tests_crypto_pwhash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_long_long
  use sodium
  block
    character(len=:), allocatable :: pwd
    character(len=SODIUM_crypto_pwhash_SALTBYTES) salt
    character(len=SODIUM_crypto_box_SEEDBYTES) key
    character(len=SODIUM_crypto_pwhash_STRBYTES) hashed_pwd
    integer ret
    ! key derivation
    pwd = "Correct Horse Battery Staple"
    call randombytes_buf(salt)
    ret = crypto_pwhash(key, pwd, salt)
    if (ret.ne.0) &
      error stop "crypto_pwhash failed"
    ret = crypto_pwhash_str(hashed_pwd, pwd)
    if (ret.ne.0) &
      error stop "crypto_pwhash_str failed"
    ret = crypto_pwhash_str_verify(hashed_pwd, pwd)
    if (ret.ne.0) &
      error stop "crypto_pwhash_str_verify failed"
  end block
end program tests_crypto_pwhash
