program tests_crypto_hash
  use, intrinsic::iso_c_binding, only : c_size_t
  use sodium
  implicit none

  block




  
  !   type(error_type), allocatable, intent(out)::error
  !   character(len=:), allocatable::msg, hash, key, m1, m2
  !   integer(kind=c_size_t)::hb, kb
  !   integer(kind=c_size_t)::klen
  !   integer(kind=c_long_long)::mlen, m1l, m2l
  !   integer::res
  !   type(crypto_generichash_blake2b_state)::state
  !   hb = crypto_generichash_bytes()
  !   kb = crypto_generichash_keybytes()
  !   allocate (character(len=hb)::hash)

    ! single part without key

    character(len=SODIUM_crypto_generichash_BYTES) :: hash
    character(len=:), allocatable :: msg
    integer(kind=c_long_long) :: mlen
    integer :: ret

  

    msg = "Arbitrary data to hash"
    mlen = len(msg)
    ! msg=c_str(msg)
  !   key = c_null_char
  !   klen = 0
    ret = crypto_generichash(hash, SODIUM_crypto_generichash_BYTES, msg, mlen, c_null_char, klen)
  !   call check(error, res, 0)
  !   ! single part with a key
  !   deallocate (key)
  !   allocate (character(len=kb)::key)
  !   call randombytes_buf(key, kb)
  !   res = crypto_generichash(hash, hb, msg, mlen, key, kb)
  !   call check(error, res, 0)
  !   call crypto_generichash_keygen(key)
  !   res = crypto_generichash(hash, hb, msg, mlen, key, kb)
  !   call check(error, res, 0)
  !   ! multi part with a key
  !   m1 = msg
  !   m2 = "is longer than expected"
  !   m1l = len(m1)
  !   m2l = len(m2)
  !   res = crypto_generichash_init(state, key, kb, hb)
  !   call check(error, res, 0)
  !   res = crypto_generichash_update(state, m1, m1l)
  !   call check(error, res, 0)
  !   res = crypto_generichash_update(state, m2, m2l)
  !   call check(error, res, 0)
  !   res = crypto_generichash_final(state, hash, hb)
  !   call check(error, res, 0)
  !   if (allocated(error)) return
  ! end subroutine test_hash

  end block

end program tests_crypto_hash
