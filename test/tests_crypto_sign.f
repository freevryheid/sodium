program tests_crypto_sign
  use, intrinsic::iso_c_binding, only : c_long_long
  use :: sodium
  use :: mod_crypto_sign_ed25519
  implicit none

  block

    integer :: ret
    character(len=SODIUM_crypto_sign_PUBLICKEYBYTES) :: pk
    character(len=SODIUM_crypto_sign_SECRETKEYBYTES) :: sk
    character(len=SODIUM_crypto_sign_BYTES) :: sig
    character(len=:), allocatable :: msg, signed_msg, unsigned_msg
    integer(kind=c_long_long) :: mlen, signed_mlen, unsigned_mlen, slen

    msg = "hello"
    mlen = len(msg)
    allocate (character(len=SODIUM_crypto_sign_BYTES+mlen) :: signed_msg)
    allocate (character(len=mlen) :: unsigned_msg)

    ! combined mode
    ret = crypto_sign_keypair(pk, sk)
    if (ret.ne.0) &
      error stop "crypto_sign_keypair failed"
    ret = crypto_sign(signed_msg, signed_mlen, msg, mlen, sk)
    if (ret.ne.0) &
      error stop "crypto_sign failed"
    ret = crypto_sign_open(unsigned_msg, unsigned_mlen, signed_msg, signed_mlen, pk)
    if (ret.ne.0) &
      error stop "crypto_sign_open failed"
    if (unsigned_msg.ne.msg) &
      error stop "crypto_sign_open failed"

    ! detached mode
    ret = crypto_sign_keypair(pk, sk)
    if (ret.ne.0) &
      error stop "crypto_sign_keypair failed"
    ret = crypto_sign_detached(sig, slen, msg, mlen, sk)
    if (ret.ne.0) &
      error stop "crypto_sign_detached failed"
    ret = crypto_sign_verify_detached(sig, msg, mlen, pk)
    if (ret.ne.0) &
      error stop "crypto_sign_verify_detached failed"

  end block

  block

    character(len=SODIUM_crypto_sign_PUBLICKEYBYTES) :: pk
    character(len=SODIUM_crypto_sign_SECRETKEYBYTES) :: sk
    character(len=SODIUM_crypto_sign_BYTES) :: sig
    character(len=:), allocatable :: m1, m2
    integer(kind=c_long_long) :: m1len, m2len, slen
    type(crypto_sign_state) :: state
    integer :: ret

    ret = crypto_sign_keypair(pk, sk)
    if (ret.ne.0) &
      error stop "crypto_sign_keypair failed"

    ! multi-part message
    m1 = "Arbitrary data to hash"
    m2 = "is longer than expected"
    m1len = len(m1)
    m2len = len(m2)

    ! + signature creation
    ret = crypto_sign_init(state)
    if (ret.ne.0) &
      error stop "crypto_sign_init failed"
    ret = crypto_sign_update(state, m1, m1len)
    if (ret.ne.0) &
      error stop "crypto_sign_update failed"
    ret = crypto_sign_update(state, m2, m2len)
    if (ret.ne.0) &
      error stop "crypto_sign_update failed"
    ret = crypto_sign_final_create(state, sig, slen, sk)
    if (ret.ne.0) &
      error stop "crypto_sign_final_create failed"

    ! + signature verification
    ret = crypto_sign_init(state)
    if (ret.ne.0) &
      error stop "crypto_sign_init failed"
    ret = crypto_sign_update(state, m1, m1len)
    if (ret.ne.0) &
      error stop "crypto_sign_update failed"
    ret = crypto_sign_update(state, m2, m2len)
    if (ret.ne.0) &
      error stop "crypto_sign_update failed"
    ret = crypto_sign_final_verify(state, sig, pk)
    if (ret.ne.0) &
      error stop "crypto_sign_final_verify failed"

  end block

  block

    character(len=SODIUM_crypto_sign_SEEDBYTES) :: sd1, sd2
    character(len=SODIUM_crypto_sign_PUBLICKEYBYTES) :: pk1, pk2
    character(len=SODIUM_crypto_sign_SECRETKEYBYTES) :: sk1, sk2
    integer :: ret

    ! extract seed and pk
    call randombytes_buf(sd1)
    ret = crypto_sign_seed_keypair(pk1, sk1, sd1)
    if (ret.ne.0) &
      error stop "crypto_sign_keypair failed"
    ret = crypto_sign_ed25519_sk_to_seed(sd2, sk1)
    if (ret.ne.0) &
      error stop "crypto_sign_ed25519_sk_to_seed failed"
    if (sd1.ne.sd2) &
      error stop "crypto_sign_ed25519_sk_to_seed failed"
    ret = crypto_sign_ed25519_sk_to_pk(pk2, sk1)
    if (ret.ne.0) &
      error stop "crypto_sign_ed25519_sk_to_pk failed"
    if (pk1.ne.pk2) &
      error stop "crypto_sign_ed25519_sk_to_pk failed"

  end block

end program tests_crypto_sign