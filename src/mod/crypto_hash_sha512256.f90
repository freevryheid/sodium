 ! public::
 ! public::
 ! public::crypto_auth_hmacsha512256_bytes
 ! public::
 ! public::crypto_auth_hmacsha512256_keybytes
 ! public::crypto_auth_hmacsha512256
 ! public::crypto_auth_hmacsha512256_verify
 ! public::crypto_auth_hmacsha512256_statebytes
 ! public::crypto_auth_hmacsha512256_init
 ! public::crypto_auth_hmacsha512256_update
 ! public::crypto_auth_hmacsha512256_final
 ! public::crypto_auth_hmacsha512256_keygen
 ! 
 ! #define crypto_auth_hmacsha512256_H
 !  :: 
 !---
 ! 
 ! #define crypto_auth_hmacsha512256_BYTES 32U
 !  :: 
 !---
 ! size_t
 ! function crypto_auth_hmacsha512256_bytes(void)bind(c,name='crypto_auth_hmacsha512256_bytes')result(res)
 !  :: void
 !---
 ! 
 ! #define crypto_auth_hmacsha512256_KEYBYTES 32U
 !  :: 
 !---
 ! size_t
 ! function crypto_auth_hmacsha512256_keybytes(void)bind(c,name='crypto_auth_hmacsha512256_keybytes')result(res)
 !  :: void
 !---
 ! int
 ! function crypto_auth_hmacsha512256(out,in,inlen,k)bind(c,name='crypto_auth_hmacsha512256')result(res)
 ! unsigned char :: out
 ! const unsigned char :: in
 ! unsigned long long :: inlen
 ! const unsigned char :: k
 !---
 ! int
 ! function crypto_auth_hmacsha512256_verify(h,in,inlen,k)bind(c,name='crypto_auth_hmacsha512256_verify')result(res)
 ! const unsigned char :: h
 ! const unsigned char :: in
 ! unsigned long long :: inlen
 ! const unsigned char :: k
 !---
 ! size_t
 ! function crypto_auth_hmacsha512256_statebytes(void)bind(c,name='crypto_auth_hmacsha512256_statebytes')result(res)
 !  :: void
 !---
 ! int
 ! function crypto_auth_hmacsha512256_init(state,key,keylen)bind(c,name='crypto_auth_hmacsha512256_init')result(res)
 ! crypto_auth_hmacsha512256_state :: state
 ! const unsigned char :: key
 ! size_t :: keylen
 !---
 ! int
 ! function crypto_auth_hmacsha512256_update(state,in,inlen)bind(c,name='crypto_auth_hmacsha512256_update')result(res)
 ! crypto_auth_hmacsha512256_state :: state
 ! const unsigned char :: in
 ! unsigned long long :: inlen
 !---
 ! int
 ! function crypto_auth_hmacsha512256_final(state,out)bind(c,name='crypto_auth_hmacsha512256_final')result(res)
 ! crypto_auth_hmacsha512256_state :: state
 ! unsigned char :: out
 !---
 ! void
 ! subroutine crypto_auth_hmacsha512256_keygen(k)bind(c,name='crypto_auth_hmacsha512256_keygen')
 ! unsigned char :: k
 !---
