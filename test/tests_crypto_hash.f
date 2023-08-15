program tests_crypto_hash
  use sodium
  use mod_common
  implicit none

  ! single part without key
  block
    character(len=SODIUM_crypto_generichash_BYTES) hash
    integer ret
    ret = crypto_generichash(hash, "Arbitrary data to hash")
    if (ret.ne.0) &
      error stop "crypto_generichash failed"
  end block

  ! single part with a key
  block
    character(len=SODIUM_crypto_generichash_KEYBYTES) key
    character(len=SODIUM_crypto_generichash_BYTES) hash
    integer ret
    call randombytes_buf(key)
    ret = crypto_generichash(hash, "Arbitrary data to hash", key)
    if (ret.ne.0) &
      error stop "crypto_generichash failed"
  end block

  ! single part with a keygen
  block
    character(len=SODIUM_crypto_generichash_KEYBYTES) key
    character(len=SODIUM_crypto_generichash_BYTES) hash
    integer ret
    call crypto_generichash_keygen(key)
    ret = crypto_generichash(hash, "Arbitrary data to hash", key)
    if (ret.ne.0) &
      error stop "crypto_generichash failed"
  end block

  ! multi part with a key
  block
    type(crypto_generichash_blake2b_state) state
    character(len=SODIUM_crypto_generichash_KEYBYTES) key
    character(len=SODIUM_crypto_generichash_BYTES) hash
    integer ret
    call crypto_generichash_keygen(key)
    ret = crypto_generichash_init(state, key)
    if (ret.ne.0) &
      error stop "crypto_generichash_init failed"
    ret = crypto_generichash_update(state, "one")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_update(state, "two")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_update(state, "three")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_final(state, hash)
    if (ret.ne.0) &
      error stop "crypto_generichash_final failed"
  end block

  ! multi part without key
  block
    type(crypto_generichash_blake2b_state) state
    character(len=SODIUM_crypto_generichash_BYTES) hash
    integer ret
    ret = crypto_generichash_init(state)
    if (ret.ne.0) &
      error stop "crypto_generichash_init failed"
    ret = crypto_generichash_update(state, "one")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_update(state, "two")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_update(state, "three")
    if (ret.ne.0) &
      error stop "crypto_generichash_update failed"
    ret = crypto_generichash_final(state, hash)
    if (ret.ne.0) &
      error stop "crypto_generichash_final failed"
  end block

end program tests_crypto_hash
