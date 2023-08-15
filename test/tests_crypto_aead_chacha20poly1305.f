program tests_crypto_aead_chacha20poly1305
  use sodium
  implicit none
  integer ret

  ! init
  ret = sodium_init()
  if (ret.ne.0) &
    error stop "sodium_init failed"

  ! ietf combined mode
  block
    character(len=SODIUM_crypto_aead_chacha20poly1305_ietf_KEYBYTES) key
    character(len=SODIUM_crypto_aead_chacha20poly1305_ietf_NPUBBYTES) nonce
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test1"
    call crypto_aead_chacha20poly1305_ietf_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)+SODIUM_crypto_aead_chacha20poly1305_ietf_ABYTES) :: ciphertext)
    ret = crypto_aead_chacha20poly1305_ietf_encrypt(ciphertext, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_ietf_encrypt failed"
    allocate (character(len=len(ciphertext)-SODIUM_crypto_aead_chacha20poly1305_ietf_ABYTES) :: dmsg) ! decrypted message
    ret = crypto_aead_chacha20poly1305_ietf_decrypt(dmsg, ciphertext, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_ietf_decrypt failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_chacha20poly1305_ietf_decrypt failed"
  end block

  ! ietf detached mode
  block
    character(len=SODIUM_crypto_aead_chacha20poly1305_ietf_KEYBYTES) key
    character(len=SODIUM_crypto_aead_chacha20poly1305_ietf_NPUBBYTES) nonce
    character(len=SODIUM_crypto_aead_chacha20poly1305_ietf_ABYTES) mac
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test2"
    call crypto_aead_chacha20poly1305_ietf_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)) :: ciphertext)
    ret = crypto_aead_chacha20poly1305_ietf_encrypt_detached(ciphertext, mac, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_ietf_encrypt_detached failed"
    allocate (character(len=len(ciphertext)) :: dmsg) ! decrypted message
    ret = crypto_aead_chacha20poly1305_ietf_decrypt_detached(dmsg, ciphertext, mac, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_ietf_decrypt_detached failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_chacha20poly1305_ietf_decrypt_detached failed"
  end block

  ! combined mode
  block
    character(len=SODIUM_crypto_aead_chacha20poly1305_KEYBYTES) key
    character(len=SODIUM_crypto_aead_chacha20poly1305_NPUBBYTES) nonce
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test1"
    call crypto_aead_chacha20poly1305_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)+SODIUM_crypto_aead_chacha20poly1305_ABYTES) :: ciphertext)
    ret = crypto_aead_chacha20poly1305_encrypt(ciphertext, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_encrypt failed"
    allocate (character(len=len(ciphertext)-SODIUM_crypto_aead_chacha20poly1305_ABYTES) :: dmsg) ! decrypted message
    ret = crypto_aead_chacha20poly1305_decrypt(dmsg, ciphertext, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_decrypt failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_chacha20poly1305_decrypt failed"
  end block

  ! detached mode
  block
    character(len=SODIUM_crypto_aead_chacha20poly1305_KEYBYTES) key
    character(len=SODIUM_crypto_aead_chacha20poly1305_NPUBBYTES) nonce
    character(len=SODIUM_crypto_aead_chacha20poly1305_ABYTES) mac
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test2"
    call crypto_aead_chacha20poly1305_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)) :: ciphertext)
    ret = crypto_aead_chacha20poly1305_encrypt_detached(ciphertext, mac, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_encrypt_detached failed"
    allocate (character(len=len(ciphertext)) :: dmsg) ! decrypted message
    ret = crypto_aead_chacha20poly1305_decrypt_detached(dmsg, ciphertext, mac, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_chacha20poly1305_decrypt_detached failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_chacha20poly1305_decrypt_detached failed"
  end block

end program tests_crypto_aead_chacha20poly1305
