program tests_crypto_shorthash
  use, intrinsic :: iso_c_binding, only : c_size_t, c_long_long
  use sodium
  block
    character(len=SODIUM_crypto_shorthash_KEYBYTES) :: key
    character(len=SODIUM_crypto_shorthash_BYTES) :: out
    integer :: ret
    call crypto_shorthash_keygen(key)
    ret = crypto_shorthash(out, "Hello World!", key)
    if (ret.ne.0) &
      error stop "crypto_shorthash failed" 
  end block
end program tests_crypto_shorthash