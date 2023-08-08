program tests_crypto_aead_aes256gcm2
  use, intrinsic::iso_c_binding, only : c_size_t
  use sodium
  implicit none

  type tests
    character(len=:), allocatable :: key_hex
    character(len=:), allocatable :: nonce_hex
    character(len=:), allocatable :: ad_hex
    character(len=:), allocatable :: message_hex
    character(len=:), allocatable :: detached_ciphertext_hex
    character(len=:), allocatable :: mac_hex
    character(len=:), allocatable :: outcome
  end type

  block

    integer :: ret, io, iostat
    character(len=64) :: a, b, c, d, e, f, g
    type(tests) :: test
    character(len=:), pointer :: key, nonce, mac, msg, ad, detached_ciphertext, decrypted
    integer(kind=c_size_t) :: msg_len, ad_len, detached_ciphertext_len

    ! init
    ret = sodium_init()
    if (ret.ne.0) &
      error stop "sodium_init failed"
    ret = crypto_aead_aes256gcm_is_available()
    if (ret.eq.0) &
      error stop "not available on this cpu"

    key => sodium_malloc(SODIUM_crypto_aead_aes256gcm_KEYBYTES)
    nonce => sodium_malloc(SODIUM_crypto_aead_aes256gcm_NPUBBYTES)
    mac => sodium_malloc(SODIUM_crypto_aead_aes256gcm_ABYTES)

    open(newunit=io, file="test/aead_aes256gcm2.data", status="old", action="read", iostat=iostat)

    do while (iostat.eq.0)
      read(unit=io, fmt='(a)', iostat=iostat) a
      read(unit=io, fmt='(a)', iostat=iostat) b
      read(unit=io, fmt='(a)', iostat=iostat) c
      read(unit=io, fmt='(a)', iostat=iostat) d
      read(unit=io, fmt='(a)', iostat=iostat) e
      read(unit=io, fmt='(a)', iostat=iostat) f
      read(unit=io, fmt='(a)', iostat=iostat) g
      test%key_hex                 = trim(a)
      test%nonce_hex               = trim(b)
      test%ad_hex                  = trim(c)
      test%message_hex             = trim(d)
      test%detached_ciphertext_hex = trim(e)
      test%mac_hex                 = trim(f)
      test%outcome                 = trim(g)
      if (iostat.eq.0) then
        key = sodium_hex2bin(test%key_hex)
        nonce = sodium_hex2bin(test%nonce_hex)
        msg_len = len(test%message_hex)/2
        msg => sodium_malloc(msg_len)
        msg = sodium_hex2bin(test%message_hex)
        ad_len = len(test%ad_hex)/2
        ad => sodium_malloc(ad_len)
        ad = sodium_hex2bin(test%ad_hex)
        mac = sodium_hex2bin(test%mac_hex)
        detached_ciphertext_len = msg_len
        detached_ciphertext => sodium_malloc(detached_ciphertext_len)
        detached_ciphertext = sodium_hex2bin(test%detached_ciphertext_hex)
        decrypted => sodium_malloc(msg_len)
        ret = crypto_aead_aes256gcm_decrypt_detached(decrypted, detached_ciphertext, mac, nonce, key, ad)
        if (ret.eq.0) then
          if (test%outcome.ne."valid") &
            error stop "test failed"
        else
          if (test%outcome.ne."invalid") &
            error stop "test failed"
         end if
        call sodium_free(msg)
        call sodium_free(ad)
        call sodium_free(decrypted)
        call sodium_free(detached_ciphertext)
      end if

    end do
    close(io)
    call sodium_free(key)
    call sodium_free(mac)
    call sodium_free(nonce)

  end block

end program tests_crypto_aead_aes256gcm2
