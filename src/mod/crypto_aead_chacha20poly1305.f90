module mod_crypto_aead_chacha20poly1305

	use,intrinsic::iso_c_binding
	use::mod_common

	implicit none

	private

	public::crypto_aead_chacha20poly1305_ietf_keybytes
	public::crypto_aead_chacha20poly1305_ietf_nsecbytes
	public::crypto_aead_chacha20poly1305_ietf_npubbytes
	public::crypto_aead_chacha20poly1305_ietf_abytes
	public::crypto_aead_chacha20poly1305_ietf_messagebytes_max
	public::crypto_aead_chacha20poly1305_ietf_encrypt
	public::crypto_aead_chacha20poly1305_ietf_decrypt
	public::crypto_aead_chacha20poly1305_ietf_encrypt_detached
	public::crypto_aead_chacha20poly1305_ietf_decrypt_detached
	public::crypto_aead_chacha20poly1305_ietf_keygen
	public::crypto_aead_chacha20poly1305_keybytes
	public::crypto_aead_chacha20poly1305_nsecbytes
	public::crypto_aead_chacha20poly1305_npubbytes
	public::crypto_aead_chacha20poly1305_abytes
	public::crypto_aead_chacha20poly1305_messagebytes_max
	public::crypto_aead_chacha20poly1305_encrypt
	public::crypto_aead_chacha20poly1305_decrypt
	public::crypto_aead_chacha20poly1305_encrypt_detached
	public::crypto_aead_chacha20poly1305_decrypt_detached
	public::crypto_aead_chacha20poly1305_keygen
!
 ! #define crypto_aead_chacha20poly1305_H
 ! #define crypto_aead_chacha20poly1305_ietf_KEYBYTES 32U
 ! #define crypto_aead_chacha20poly1305_ietf_NSECBYTES 0U
 ! #define crypto_aead_chacha20poly1305_ietf_NPUBBYTES 12U
 ! #define crypto_aead_chacha20poly1305_ietf_ABYTES 16U
 ! #define crypto_aead_chacha20poly1305_KEYBYTES 32U
 ! #define crypto_aead_chacha20poly1305_NSECBYTES 0U
 ! #define crypto_aead_chacha20poly1305_NPUBBYTES 8U
 ! #define crypto_aead_chacha20poly1305_ABYTES 16U
 ! #define crypto_aead_chacha20poly1305_IETF_KEYBYTES crypto_aead_chacha20poly1305_ietf_KEYBYTES
 ! #define crypto_aead_chacha20poly1305_IETF_NSECBYTES crypto_aead_chacha20poly1305_ietf_NSECBYTES
 ! #define crypto_aead_chacha20poly1305_IETF_NPUBBYTES crypto_aead_chacha20poly1305_ietf_NPUBBYTES
 ! #define crypto_aead_chacha20poly1305_IETF_ABYTES crypto_aead_chacha20poly1305_ietf_ABYTES
 ! #define crypto_aead_chacha20poly1305_IETF_MESSAGEBYTES_MAX crypto_aead_chacha20poly1305_ietf_MESSAGEBYTES_MAX

	interface

		function crypto_aead_chacha20poly1305_ietf_keybytes()bind(c,name='crypto_aead_chacha20poly1305_ietf_keybytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_aead_chacha20poly1305_ietf_keybytes

		function crypto_aead_chacha20poly1305_ietf_nsecbytes()bind(c,name='crypto_aead_chacha20poly1305_ietf_nsecbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_aead_chacha20poly1305_ietf_nsecbytes

		function crypto_aead_chacha20poly1305_ietf_npubbytes()bind(c,name='crypto_aead_chacha20poly1305_ietf_npubbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_aead_chacha20poly1305_ietf_npubbytes

		function crypto_aead_chacha20poly1305_ietf_abytes()bind(c,name='crypto_aead_chacha20poly1305_ietf_abytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_aead_chacha20poly1305_ietf_abytes

		function crypto_aead_chacha20poly1305_ietf_messagebytes_max()bind(c,name='crypto_aead_chacha20poly1305_ietf_messagebytes_max')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_aead_chacha20poly1305_ietf_messagebytes_max

		function bind_crypto_aead_chacha20poly1305_ietf_encrypt(c,clen_p,m,mlen,ad,adlen,nsec,npub,k)bind(c,name='crypto_aead_chacha20poly1305_ietf_encrypt')result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char),intent(inout)::c
			integer(kind=c_long_long),value,intent(in)::clen_p,mlen,adlen
			character(kind=c_char),intent(in)::m,ad,nsec,npub,k
		end function bind_crypto_aead_chacha20poly1305_ietf_encrypt

		function bind_crypto_aead_chacha20poly1305_ietf_decrypt(m,mlen_p,nsec,c,clen,ad,adlen,npub,k)bind(c,name='crypto_aead_chacha20poly1305_ietf_decrypt')result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char),intent(inout)::m
			integer(kind=c_long_long),value,intent(in)::mlen_p,clen,adlen
			character(kind=c_char),intent(in)::nsec,c,ad,npub,k
		end function bind_crypto_aead_chacha20poly1305_ietf_decrypt

 !---
 ! int
 ! function crypto_aead_chacha20poly1305_ietf_encrypt_detached(c,mac,maclen_p,m,mlen,ad,adlen,nsec,npub,k)bind(c,name='crypto_aead_chacha20poly1305_ietf_encrypt_detached')result(res)
 ! unsigned char :: c
 ! unsigned char :: mac
 ! unsigned long long :: maclen_p
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: nsec
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! int
 ! function crypto_aead_chacha20poly1305_ietf_decrypt_detached(m,nsec,c,clen,mac,ad,adlen,npub,k)bind(c,name='crypto_aead_chacha20poly1305_ietf_decrypt_detached')result(res)
 ! unsigned char :: m
 ! unsigned char :: nsec
 ! const unsigned char :: c
 ! unsigned long long :: clen
 ! const unsigned char :: mac
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! void
 ! subroutine crypto_aead_chacha20poly1305_ietf_keygen(k)bind(c,name='crypto_aead_chacha20poly1305_ietf_keygen')
 ! unsigned char :: k
 !---
 !

 !  ::
 !---
 ! size_t
 ! function crypto_aead_chacha20poly1305_keybytes(void)bind(c,name='crypto_aead_chacha20poly1305_keybytes')result(res)
 !  :: void
 !---
 !

 !  ::
 !---
 ! size_t
 ! function crypto_aead_chacha20poly1305_nsecbytes(void)bind(c,name='crypto_aead_chacha20poly1305_nsecbytes')result(res)
 !  :: void
 !---
 !

 !  ::
 !---
 ! size_t
 ! function crypto_aead_chacha20poly1305_npubbytes(void)bind(c,name='crypto_aead_chacha20poly1305_npubbytes')result(res)
 !  :: void
 !---
 !

 !  ::
 !---
 ! size_t
 ! function crypto_aead_chacha20poly1305_abytes(void)bind(c,name='crypto_aead_chacha20poly1305_abytes')result(res)
 !  :: void
 !---
 ! size_t
 ! function crypto_aead_chacha20poly1305_messagebytes_max(void)bind(c,name='crypto_aead_chacha20poly1305_messagebytes_max')result(res)
 !  :: void
 !---
 ! int
 ! function crypto_aead_chacha20poly1305_encrypt(c,clen_p,m,mlen,ad,adlen,nsec,npub,k)bind(c,name='crypto_aead_chacha20poly1305_encrypt')result(res)
 ! unsigned char :: c
 ! unsigned long long :: clen_p
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: nsec
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! int
 ! function crypto_aead_chacha20poly1305_decrypt(m,mlen_p,nsec,c,clen,ad,adlen,npub,k)bind(c,name='crypto_aead_chacha20poly1305_decrypt')result(res)
 ! unsigned char :: m
 ! unsigned long long :: mlen_p
 ! unsigned char :: nsec
 ! const unsigned char :: c
 ! unsigned long long :: clen
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! int
 ! function crypto_aead_chacha20poly1305_encrypt_detached(c,mac,maclen_p,m,mlen,ad,adlen,nsec,npub,k)bind(c,name='crypto_aead_chacha20poly1305_encrypt_detached')result(res)
 ! unsigned char :: c
 ! unsigned char :: mac
 ! unsigned long long :: maclen_p
 ! const unsigned char :: m
 ! unsigned long long :: mlen
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: nsec
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! int
 ! function crypto_aead_chacha20poly1305_decrypt_detached(m,nsec,c,clen,mac,ad,adlen,npub,k)bind(c,name='crypto_aead_chacha20poly1305_decrypt_detached')result(res)
 ! unsigned char :: m
 ! unsigned char :: nsec
 ! const unsigned char :: c
 ! unsigned long long :: clen
 ! const unsigned char :: mac
 ! const unsigned char :: ad
 ! unsigned long long :: adlen
 ! const unsigned char :: npub
 ! const unsigned char :: k
 !---
 ! void
 ! subroutine crypto_aead_chacha20poly1305_keygen(k)bind(c,name='crypto_aead_chacha20poly1305_keygen')
 ! unsigned char :: k
 !---
 !

 !  ::
 !---
 !

 !  ::
 !---
 !

 !  ::
 !---
 !

 !  ::
 !---
 !

 !  ::
 !---

		function crypto_aead_chacha20poly1305_ietf_encrypt(c,clen_p,m,mlen,ad,adlen,nsec,npub,k)result(res)
			integer(kind=c_int)::res
			character(len=:),allocatable,intent(inout)::c
			integer(kind=c_long_long),intent(in)::clen_p,mlen,adlen
			character(len=:),allocatable,intent(in)::m,ad,nsec,npub,k
			ret=bind_crypto_aead_chacha20poly1305_ietf_encrypt(c,clen_p,m,mlen,ad,adlen,nsec,npub,k)
		end function crypto_aead_chacha20poly1305_ietf_encrypt

		function crypto_aead_chacha20poly1305_ietf_decrypt(m,mlen_p,nsec,c,clen,ad,adlen,npub,k)result(res)
			integer(kind=c_int)::res
			character(len=:),allocatable,intent(inout)::m
			integer(kind=c_long_long),intent(in)::mlen_p,clen,adlen
			character(len=:),allocatable,intent(in)::nsec,c,ad,npub,k
			res=bind_crypto_aead_chacha20poly1305_ietf_decrypt(m,mlen_p,nsec,c,clen,ad,adlen,npub,k)
		end function crypto_aead_chacha20poly1305_ietf_decrypt





endmodule mod_crypto_aead_chacha20poly1305
