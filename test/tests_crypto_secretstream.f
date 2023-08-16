program tests_crypto_secretstream
  use, intrinsic :: iso_c_binding, only : c_long_long, c_null_char, c_char, c_signed_char
  use mod_crypto_secretstream_xchacha20poly1305
  use mod_common
  implicit none
  block
    type(crypto_secretstream_xchacha20poly1305_state) state
    character(len=SODIUM_crypto_secretstream_xchacha20poly1305_KEYBYTES) key
    character(len=SODIUM_crypto_secretstream_xchacha20poly1305_HEADERBYTES) header
    character(len=*), parameter :: m1 = "Arbitrary data to encrypt"
    character(len=*), parameter :: m2 = "split into"
    character(len=*), parameter :: m3 = "three messages"
    integer(kind=c_long_long) m1l, m2l, m3l, r1l, r2l, r3l
    integer ret
    character(len=:), allocatable :: c1, c2, c3, r1, r2, r3
    integer(kind=c_signed_char) tag

    m1l = len(m1)
    m2l = len(m2)
    m3l = len(m3)
    r1l = m1l + SODIUM_crypto_secretstream_xchacha20poly1305_ABYTES
    r2l = m2l + SODIUM_crypto_secretstream_xchacha20poly1305_ABYTES
    r3l = m3l + SODIUM_crypto_secretstream_xchacha20poly1305_ABYTES
    allocate (character(len=r1l) :: c1)
    allocate (character(len=r2l) :: c2)
    allocate (character(len=r3l) :: c3)
    allocate (character(len=m1l) :: r1)
    allocate (character(len=m2l) :: r2)
    allocate (character(len=m3l) :: r3)
    ! shared secret key required to encrypt/decrypt the stream
    call crypto_secretstream_xchacha20poly1305_keygen(key)
    ! encrypt
    ! set up a new stream: initialize the state and create the header
    ret = crypto_secretstream_xchacha20poly1305_init_push(state, header, key)
    if (ret.ne.0) &
      error stop "init_push failed"
    ! encrypt the first chunk.
    ! c1 will contain an encrypted, authenticated representation of m1
    ret = crypto_secretstream_xchacha20poly1305_push(state, c1, m1, &
      SODIUM_crypto_secretstream_xchacha20poly1305_TAG_MESSAGE)
    if (ret.ne.0) &
      error stop "push1 failed"
    ! encrypt the second chunk.
    ! c2 will contain an encrypted, authenticated representation of m2
    ret = crypto_secretstream_xchacha20poly1305_push(state, c2, m2, &
      SODIUM_crypto_secretstream_xchacha20poly1305_TAG_MESSAGE)
     if (ret.ne.0) &
      error stop "push2 failed"
    ! encrypt the last chunk.
    ! c3 will contain an encrypted, authenticated representation of m3
    ! note the `TAG_FINAL` tag to indicate that this is the final chunk.
    ret = crypto_secretstream_xchacha20poly1305_push(state, c3, m3, &
      SODIUM_crypto_secretstream_xchacha20poly1305_TAG_FINAL)
    if (ret.ne.0) &
      error stop "push3 failed"
    ! decrypt
    ! decrypt the stream: initializes the state, using the key and a header
    ret = crypto_secretstream_xchacha20poly1305_init_pull(state, header, key)
    if (ret.ne.0) &
      error stop "init_pull failed"
    ! decrypt the first chunk. A real application would probably use
    ! a loop, that reads data from the network or from disk, and exits after
    ! an error, or after the last chunk (with a TAG_FINAL tag) has been
    ! decrypted.
    ret = crypto_secretstream_xchacha20poly1305_pull(state, r1, tag, c1)
    if (ret.ne.0) &
      error stop "pull1 failed"
    if (tag.ne.SODIUM_crypto_secretstream_xchacha20poly1305_TAG_MESSAGE) &
      error stop "tag1 failed"
    if (r1.ne.m1) &
      error stop "decrypt1 failed"
    ! decrypt the second chunk, store the result into r2
    ret = crypto_secretstream_xchacha20poly1305_pull(state, r2, tag, c2)
    if (ret.ne.0) &
      error stop "pull2 failed"
    if (tag.ne.SODIUM_crypto_secretstream_xchacha20poly1305_TAG_MESSAGE) &
      error stop "tag2 failed"
    if (r2.ne.m2) &
      error stop "decrypt2 failed"
    ! decrypt the last chunk, store the result into r3
    ret = crypto_secretstream_xchacha20poly1305_pull(state, r3, tag, c3)
    if (ret.ne.0) &
      error stop "pull3 failed"
    if (tag.ne.SODIUM_crypto_secretstream_xchacha20poly1305_TAG_FINAL) &
      error stop "tag3 failed"
    if (r3.ne.m3) &
      error stop "decrypt3 failed"

    ! the tag indicates that this is the final chunk, no need to read and decrypt more

  end block

end program tests_crypto_secretstream
