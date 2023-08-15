program tests_crypto_onetimeauth
  use, intrinsic :: iso_c_binding, only : c_size_t, c_long_long
  use sodium
  block
    character(len=SODIUM_crypto_onetimeauth_KEYBYTES) key
    character(len=SODIUM_crypto_onetimeauth_BYTES) out
    integer ret
    call crypto_onetimeauth_keygen(key)
    ret = crypto_onetimeauth(out, "Hello World!", key)
    if (ret.ne.0) &
      error stop "crypto_onetimeauth failed"
    ret = crypto_onetimeauth_verify(out, "Hello World!", key)
    if (ret.ne.0) &
      error stop "crypto_onetimeauth_verify failed"
  end block
end program tests_crypto_onetimeauth
