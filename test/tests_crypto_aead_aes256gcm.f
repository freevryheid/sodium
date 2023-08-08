program tests_crypto_aead_aes256gcm
  use sodium
  implicit none
  integer :: ret

  ! init
  ret = sodium_init()
  if (ret.ne.0) &
    error stop "sodium_init failed"
  ret = crypto_aead_aes256gcm_is_available()
  if (ret.eq.0) &
    error stop "not available on this cpu"

  ! combined mode
  block
    character(len=SODIUM_crypto_aead_aes256gcm_KEYBYTES) :: key
    character(len=SODIUM_crypto_aead_aes256gcm_NPUBBYTES) :: nonce
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test1"
    call crypto_aead_aes256gcm_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)+SODIUM_crypto_aead_aes256gcm_ABYTES) :: ciphertext)
    ret = crypto_aead_aes256gcm_encrypt(ciphertext, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_aes256gcm_encrypt failed"
    allocate (character(len=len(ciphertext)-SODIUM_crypto_aead_aes256gcm_ABYTES) :: dmsg) ! decrypted message
    ret = crypto_aead_aes256gcm_decrypt(dmsg, ciphertext, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_aes256gcm_decrypt failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_aes256gcm_decrypt failed"
  end block

  ! detached mode
  block
    character(len=SODIUM_crypto_aead_aes256gcm_KEYBYTES) :: key
    character(len=SODIUM_crypto_aead_aes256gcm_NPUBBYTES) :: nonce
    character(len=SODIUM_crypto_aead_aes256gcm_ABYTES) :: mac
    character(len=:), allocatable :: msg, ciphertext, dmsg
    msg = "test2"
    call crypto_aead_aes256gcm_keygen(key)
    call randombytes_buf(nonce)
    allocate (character(len=len(msg)) :: ciphertext)
    ret = crypto_aead_aes256gcm_encrypt_detached(ciphertext, mac, msg, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_aes256gcm_encrypt_detached failed"
    allocate (character(len=len(ciphertext)) :: dmsg) ! decrypted message
    ret = crypto_aead_aes256gcm_decrypt_detached(dmsg, ciphertext, mac, nonce, key)
    if (ret.ne.0) &
      error stop "crypto_aead_aes256gcm_decrypt_detached failed"
    if (msg.ne.dmsg) &
      error stop "crypto_aead_aes256gcm_decrypt_detached failed"
  end block

end program tests_crypto_aead_aes256gcm
