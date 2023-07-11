program tests_crypto_box
  use, intrinsic :: iso_c_binding, only : c_long_long, c_size_t
  use :: mod_crypto_scalarmult
  use sodium
  implicit none

  block
    character(len=PARAM_crypto_box_PUBLICKEYBYTES) :: pk
    character(len=PARAM_crypto_box_SECRETKEYBYTES) :: sk
    character(len=PARAM_crypto_box_PUBLICKEYBYTES) :: bob_pk
    character(len=PARAM_crypto_box_SECRETKEYBYTES) :: bob_sk
    character(len=PARAM_crypto_box_PUBLICKEYBYTES) :: alice_pk
    character(len=PARAM_crypto_box_SECRETKEYBYTES) :: alice_sk
    character(len=PARAM_crypto_box_SEEDBYTES) :: seed
    character(len=PARAM_crypto_box_NONCEBYTES) :: nonce
    character(len=:), allocatable :: msg, cipher, decrypted
    integer(kind=c_long_long) :: ml, cl
    integer :: res
    call randombytes_buf(seed)
    call randombytes_buf(nonce)
    res = crypto_box_seed_keypair(pk, sk, seed)
    if (res.ne.0) &
      error stop "error: crypto_box_seed_keypair failed"
    res = crypto_box_keypair(alice_pk, alice_sk)
    if (res.ne.0) &
      error stop "error: crypto_box_keypair failed"
    res = crypto_box_keypair(bob_pk, bob_sk)
    if (res.ne.0) &
      error stop "error: crypto_box_keypair failed"
    msg = "test"
    ml = len(msg)
    cl = ml + PARAM_crypto_box_MACBYTES
    allocate (character(len=cl)::cipher)
    allocate (character(len=ml)::decrypted)
    res = crypto_box_easy(cipher, c_str(msg), ml, nonce, bob_pk, alice_sk) ! note this wants a c-string
    if (res.ne.0) &
      error stop "error: crypto_box_easy"
    res = crypto_box_open_easy(decrypted, cipher, cl, nonce, alice_pk, bob_sk)
    if (res.ne.0) &
      error stop "error: crypto_box_open_easy"
    if (decrypted.ne.msg) &
      error stop "error: crypto_box_open_easy"
    res = crypto_scalarmult_base(alice_pk, bob_sk) ! retrieve public key from secret key previously generated using crypto_key_pair
    if (res.ne.0) &
      error stop "error: crypto_scalarmult_base"
  end block

  block
    character(len=PARAM_crypto_box_PUBLICKEYBYTES) :: rpk
    character(len=PARAM_crypto_box_SECRETKEYBYTES) :: rsk
    character(len=:), allocatable :: m, c, d
    integer(kind=c_long_long) :: clen, mlen
    integer(kind=c_size_t) :: sbb
    integer :: res
    sbb = crypto_box_sealbytes() ! we don't have a PARAM for this (yet)
    res = crypto_box_keypair(rpk, rsk)
    if (res.ne.0) &
      error stop "error: crypto_box_keypair failed"
    m = "Secret message"
    mlen = len(m)
    clen = mlen + sbb
    allocate (character(len=clen) :: c)
    allocate (character(len=mlen) :: d)
    res = crypto_box_seal(c, m, mlen, rpk)
    if (res.ne.0) &
      error stop "error: crypto_box_seal"
    res = crypto_box_seal_open(d, c, clen, rpk, rsk)
    if (res.ne.0) &
      error stop "error: crypto_box_seal_open"
    if (d.ne.m) &
      error stop "error: crypto_box_seal_open"
  end block

end program tests_crypto_box
