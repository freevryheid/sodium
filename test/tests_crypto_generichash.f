program tests_crypto_generichash
  use, intrinsic::iso_c_binding, only : c_size_t 
  use sodium
  implicit none

  type tests
    character(len=:), allocatable :: in_hex
    character(len=:), allocatable :: key_hex
    character(len=:), allocatable :: out_hex
  end type

  block
    character(len=128) :: a, b, c
    integer :: ret, io, iostat
    type(tests) :: test
    character(len=:), pointer :: key, expected_out, out, in
    integer(kind=c_size_t) :: hlen
    ret = sodium_init()
    if (ret.ne.0) &
      error stop "sodium_init failed"
    key => sodium_malloc(SODIUM_crypto_generichash_KEYBYTES_MAX)
    out => sodium_malloc(SODIUM_crypto_generichash_BYTES_MAX)
    expected_out => sodium_malloc(SODIUM_crypto_generichash_BYTES_MAX)
    open(newunit=io, file="test/tests_crypto_generichash.data", status="old", action="read", iostat=iostat)
    do while (iostat.eq.0)
      read(unit=io, fmt='(a)', iostat=iostat) a
      read(unit=io, fmt='(a)', iostat=iostat) b
      read(unit=io, fmt='(a)', iostat=iostat) c
      test%in_hex = trim(a)
      test%key_hex = trim(b)
      test%out_hex = trim(c)
      if (iostat.eq.0) then
        key = sodium_hex2bin(test%key_hex)
        expected_out = sodium_hex2bin(test%out_hex)
        hlen = len(test%in_hex)
        in => sodium_malloc(hlen)
        in = sodium_hex2bin(test%in_hex)
        ret = crypto_generichash(out, in, key) ! out hash here
        if (ret.ne.0) &
          error stop "crypto_generichash failed 1"
        ! print *, "in: ", out
        ! print *, "out: ", expected_out
        if (out.ne.expected_out) &
          error stop "crypto_generichash failed 2"
        call sodium_free(in)
      end if
    end do
    close(io)
    call sodium_free(key)
    call sodium_free(out)
    call sodium_free(expected_out)
  end block

end program tests_crypto_generichash