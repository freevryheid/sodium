module tests_crypto_aead_aes256gcm2

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive
	use stdlib_io
	! use, intrinsic :: iso_fortran_env, only: input_unit

	implicit none

	private

	type::tests
		character(len=:),allocatable::key_hex
		character(len=:),allocatable::nonce_hex
		character(len=:),allocatable::ad_hex
		character(len=:),allocatable::message_hex
		character(len=:),allocatable::detached_ciphertext_hex
		character(len=:),allocatable::mac_hex
		character(len=:),allocatable::outcome
	endtype

	public::collect_tests_crypto_aead_aes256gcm2

	contains

		subroutine collect_tests_crypto_aead_aes256gcm2(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for aead_aes256gcm2",test_aead_aes256gcm2)]
		endsubroutine collect_tests_crypto_aead_aes256gcm2

		subroutine test_aead_aes256gcm2(error)
			type(error_type),allocatable,intent(out)::error
			type(tests)::test
			integer::u,iostat
			character(len=:),pointer::key,nonce,msg,ad,mac,detached_ciphertext,decrypted
			! type(c_ptr)::pkey,pnonce,pmsg,pad,pmac,pdetached_ciphertext,pdecrypted
			integer(kind=c_size_t)::kb,pb,ab,msg_len,ad_len,detached_ciphertext_len
			integer(kind=c_size_t),pointer::binlen=>null()
			integer::ret

			ret=sodium_init()
			call check(error,ret.ne.-1,"init")
			ret=crypto_aead_aes256gcm_is_available()
			call check(error,ret,1)

			kb=crypto_aead_aes256gcm_keybytes()
			pb=crypto_aead_aes256gcm_npubbytes()
			ab=crypto_aead_aes256gcm_abytes()

			call sodium_malloc(key,kb)
			call sodium_malloc(nonce,pb)
			call sodium_malloc(mac,ab)

			u=open('test/aead_aes256gcm2.data',iostat=iostat)
			do while(iostat.eq.0)

				call getline(u,test%key_hex,iostat)
				call getline(u,test%nonce_hex,iostat)
				call getline(u,test%ad_hex,iostat)
				call getline(u,test%message_hex,iostat)
				call getline(u,test%detached_ciphertext_hex,iostat)
				call getline(u,test%mac_hex,iostat)
				call getline(u,test%outcome,iostat)

				if(iostat.eq.0)then
					ret=sodium_hex2bin(key,kb,test%key_hex,len(test%key_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					ret=sodium_hex2bin(nonce,pb,test%nonce_hex,len(test%nonce_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					msg_len=len(test%message_hex)/2
					call sodium_malloc(msg,msg_len)
					ret=sodium_hex2bin(msg,msg_len,test%message_hex,len(test%message_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					ad_len=len(test%ad_hex)/2
					call sodium_malloc(ad,ad_len)
					ret=sodium_hex2bin(ad,ad_len,test%ad_hex,len(test%ad_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					ret=sodium_hex2bin(mac,ab,test%mac_hex,len(test%mac_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					detached_ciphertext_len=msg_len
					call sodium_malloc(detached_ciphertext,detached_ciphertext_len)
					ret=sodium_hex2bin(detached_ciphertext,detached_ciphertext_len,test%detached_ciphertext_hex,&
					&len(test%detached_ciphertext_hex,kind=c_size_t),c_null_char,binlen,c_null_ptr)
					call check(error,ret,0)
					call sodium_malloc(decrypted,msg_len)
					ret=crypto_aead_aes256gcm_decrypt_detached(decrypted,c_null_char,detached_ciphertext,&
					&detached_ciphertext_len,mac,ad,ad_len,nonce,key)

					if(ret.eq.0)then
						call check(error,test%outcome,"valid")
					else
						call check(error,test%outcome,"invalid")
					endif

					call sodium_free(msg)
					call sodium_free(ad)
					call sodium_free(decrypted)
					call sodium_free(detached_ciphertext)

				endif

			enddo
			close(u)

			call sodium_free(key)
			call sodium_free(mac)
			call sodium_free(nonce)

			if (allocated(error)) return

		endsubroutine test_aead_aes256gcm2

endmodule tests_crypto_aead_aes256gcm2
