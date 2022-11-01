 ! public::
 ! public::crypto_sign_ed25519ph_statebytes
 ! public::
 ! public::crypto_sign_ed25519_bytes
 ! public::
 ! public::crypto_sign_ed25519_seedbytes
 ! public::
 ! public::crypto_sign_ed25519_publickeybytes
 ! public::
 ! public::crypto_sign_ed25519_secretkeybytes
 ! public::
 ! public::crypto_sign_ed25519_messagebytes_max
 ! public::crypto_sign_ed25519
 ! public::crypto_sign_ed25519_open
 ! public::crypto_sign_ed25519_detached
 ! public::crypto_sign_ed25519_verify_detached
 ! public::crypto_sign_ed25519_keypair
 ! public::crypto_sign_ed25519_seed_keypair
 ! public::crypto_sign_ed25519_pk_to_curve25519
 ! public::crypto_sign_ed25519_sk_to_curve25519
 ! public::crypto_sign_ed25519_sk_to_seed
 ! public::crypto_sign_ed25519_sk_to_pk
 ! public::crypto_sign_ed25519ph_init
 ! public::crypto_sign_ed25519ph_update
 ! public::crypto_sign_ed25519ph_final_create
 ! public::crypto_sign_ed25519ph_final_verify
 !
 ! #define crypto_sign_ed25519_H
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519ph_statebytes(void)bind(c,name='crypto_sign_ed25519ph_statebytes')result(res)
 !  :: void
 !---
 !
 ! #define crypto_sign_ed25519_BYTES 64U
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519_bytes(void)bind(c,name='crypto_sign_ed25519_bytes')result(res)
 !  :: void
 !---
 !
 ! #define crypto_sign_ed25519_SEEDBYTES 32U
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519_seedbytes(void)bind(c,name='crypto_sign_ed25519_seedbytes')result(res)
 !  :: void
 !---
 !
 ! #define crypto_sign_ed25519_PUBLICKEYBYTES 32U
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519_publickeybytes(void)bind(c,name='crypto_sign_ed25519_publickeybytes')result(res)
 !  :: void
 !---
 !
 ! #define crypto_sign_ed25519_SECRETKEYBYTES (32U + 32U)
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519_secretkeybytes(void)bind(c,name='crypto_sign_ed25519_secretkeybytes')result(res)
 !  :: void
 !---
 !
 ! #define crypto_sign_ed25519_MESSAGEBYTES_MAX (SODIUM_SIZE_MAX - crypto_sign_ed25519_BYTES)
 !  ::
 !---
 ! size_t
 ! function crypto_sign_ed25519_messagebytes_max(void)bind(c,name='crypto_sign_ed25519_messagebytes_max')result(res)
 !  :: void
 !---
 ! int
 ! function crypto_sign_ed25519(sm,smlen_p,m,mlen,sk)bind(c,name='crypto_sign_ed25519')result(res)
 ! unsigned char :: sm
 ! unsigned long long :: smlen_p
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519_open(m,mlen_p,sm,smlen,pk)bind(c,name='crypto_sign_ed25519_open')result(res)
 ! unsigned char :: m
 ! unsigned long long :: mlen_p
 ! const unsigned char :: sm
 ! unsigned long long :: smlen
 ! const unsigned char :: pk
 !---
 ! int
 ! function crypto_sign_ed25519_detached(sig,siglen_p,m,mlen,sk)bind(c,name='crypto_sign_ed25519_detached')result(res)
 ! unsigned char :: sig
 ! unsigned long long :: siglen_p
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519_verify_detached(sig,m,mlen,pk)bind(c,name='crypto_sign_ed25519_verify_detached')result(res)
 ! const unsigned char :: sig
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: pk
 !---
 ! int
 ! function crypto_sign_ed25519_keypair(pk,sk)bind(c,name='crypto_sign_ed25519_keypair')result(res)
 ! unsigned char :: pk
 ! unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519_seed_keypair(pk,sk,seed)bind(c,name='crypto_sign_ed25519_seed_keypair')result(res)
 ! unsigned char :: pk
 ! unsigned char :: sk
 ! const unsigned char :: seed
 !---
 ! int
 ! function crypto_sign_ed25519_pk_to_curve25519(curve25519_pk,ed25519_pk)bind(c,name='crypto_sign_ed25519_pk_to_curve25519')result(res)
 ! unsigned char :: curve25519_pk
 ! const unsigned char :: ed25519_pk
 !---
 ! int
 ! function crypto_sign_ed25519_sk_to_curve25519(curve25519_sk,ed25519_sk)bind(c,name='crypto_sign_ed25519_sk_to_curve25519')result(res)
 ! unsigned char :: curve25519_sk
 ! const unsigned char :: ed25519_sk
 !---
 ! int
 ! function crypto_sign_ed25519_sk_to_seed(seed,sk)bind(c,name='crypto_sign_ed25519_sk_to_seed')result(res)
 ! unsigned char :: seed
 ! const unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519_sk_to_pk(pk,sk)bind(c,name='crypto_sign_ed25519_sk_to_pk')result(res)
 ! unsigned char :: pk
 ! const unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519ph_init(state)bind(c,name='crypto_sign_ed25519ph_init')result(res)
 ! crypto_sign_ed25519ph_state :: state
 !---
 ! int
 ! function crypto_sign_ed25519ph_update(state,m,mlen)bind(c,name='crypto_sign_ed25519ph_update')result(res)
 ! crypto_sign_ed25519ph_state :: state
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 !---
 ! int
 ! function crypto_sign_ed25519ph_final_create(state,sig,siglen_p,sk)bind(c,name='crypto_sign_ed25519ph_final_create')result(res)
 ! crypto_sign_ed25519ph_state :: state
 ! unsigned char :: sig
 ! unsigned long long :: siglen_p
 ! const unsigned char :: sk
 !---
 ! int
 ! function crypto_sign_ed25519ph_final_verify(state,sig,pk)bind(c,name='crypto_sign_ed25519ph_final_verify')result(res)
 ! crypto_sign_ed25519ph_state :: state
 ! const unsigned char :: sig
 ! const unsigned char :: pk
 !---
