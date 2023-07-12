program tests_crypto_scalarmult
  use, intrinsic::iso_c_binding, only : c_size_t, c_signed_char
  use sodium
  implicit none
  character(len=32) :: alicesk, bobsk, small_order_p
  character(len=:), allocatable::hex, res
  integer(kind=c_signed_char), dimension(32) :: a, b, c
  integer(kind=c_signed_char) :: z
  integer(kind=c_size_t) :: siz, hexlen
  character(len=:), pointer :: alicepk, bobpk, k
  integer :: ret
  data a/z'77', z'07', z'6d', z'0a', z'73', z'18', z'a5', z'7d',&
    z'3c', z'16', z'c1', z'72', z'51', z'b2', z'66', z'45',&
    z'df', z'4c', z'2f', z'87', z'eb', z'c0', z'99', z'2a',&
    z'b1', z'77', z'fb', z'a5', z'1d', z'b9', z'2c', z'2a'/
  data b/z'5d', z'ab', z'08', z'7e', z'62', z'4a', z'8a', z'4b',&
    z'79', z'e1', z'7f', z'8b', z'83', z'80', z'0e', z'e6',&
    z'6f', z'3b', z'b1', z'29', z'26', z'18', z'b6', z'fd',&
    z'1c', z'2f', z'8b', z'27', z'ff', z'88', z'e0', z'eb'/
  data c/z'e0', z'eb', z'7a', z'7c', z'3b', z'41', z'b8', z'ae',&
    z'16', z'56', z'e3', z'fa', z'f1', z'9f', z'c4', z'6a',&
    z'da', z'09', z'8d', z'eb', z'9c', z'32', z'b1', z'fd',&
    z'86', z'62', z'05', z'16', z'5f', z'49', z'b8', z'00'/
  data z/z'80'/
  hexlen = PARAM_crypto_scalarmult_BYTES*2 + 1
  ret = sodium_init()
  if(ret.ne.0) &
    error stop "error: sodium_init"
  alicesk = transfer(a, alicesk)
  bobsk = transfer(b, bobsk)
  small_order_p = transfer(c, small_order_p)
  siz = PARAM_crypto_scalarmult_BYTES
  alicepk => sodium_malloc(siz)
  bobpk => sodium_malloc(siz)
  k => sodium_malloc(siz)
  ret = crypto_scalarmult_base(alicepk, alicesk)
  if(ret.ne.0) &
    error stop "error: crypto_scalarmult_base"
  hex = sodium_bin2hex(alicepk)
  deallocate(hex)
  ret = crypto_scalarmult_base(bobpk, bobsk)
  if(ret.ne.0) &
    error stop "error: crypto_scalarmult_base"
  ret = crypto_scalarmult(k, alicesk, bobpk)
  if(ret.ne.0) &
    error stop "error: crypto_scalarmult_base"
  ret = crypto_scalarmult(k, bobsk, alicepk)
  if(ret.ne.0) &
    error stop "error: crypto_scalarmult_base"
  alicepk(32:32) = char(z)
  ret = crypto_scalarmult(k, bobsk, alicepk)
  if(ret.ne.0) &
    error stop "error: crypto_scalarmult_base"
  ret = crypto_scalarmult(k, bobsk, small_order_p)
  if(ret.ne.-1) &
    error stop "error: crypto_scalarmult_base"
  call sodium_free(alicepk)
  call sodium_free(bobpk)
  call sodium_free(k)
  if(crypto_scalarmult_primitive().ne."curve25519") &
    error stop "error: crypto_scalarmult_primitive"
end program tests_crypto_scalarmult
