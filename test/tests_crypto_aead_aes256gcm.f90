module tests_crypto_aead_aes256gcm

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_aead_aes256gcm

	contains

		subroutine collect_tests_crypto_aead_aes256gcm(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for aead_aes256gcm",test_aead_aes256gcm)]
		endsubroutine collect_tests_crypto_aead_aes256gcm

		subroutine test_aead_aes256gcm(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::msg,key,nonce,ciphertext,ad,dmsg
			integer(kind=c_size_t)::kb,nb,ab
			integer(kind=c_long_long),pointer::clen,dlen
			integer(kind=c_long_long)::mlen,adlen
			integer::ret
			ret=sodium_init()
			call check(error,ret.ne.-1,"init")
			ret=crypto_aead_aes256gcm_is_available()
			if(ret.eq.0) error stop "not available on this cpu"
			! combined mode
			kb=crypto_aead_aes256gcm_keybytes()
			nb=crypto_aead_aes256gcm_npubbytes()
			ab=crypto_aead_aes256gcm_abytes()
			msg="test"
			ad="123456"
			mlen=len(msg)
			adlen=len(ad)
			allocate(clen)
			allocate(dlen)
			allocate(character(len=kb)::key)
			allocate(character(len=nb)::nonce)
			allocate(character(len=mlen+ab)::ciphertext)
			allocate(character(len=mlen)::dmsg) ! decrypted message
			call crypto_aead_aes256gcm_keygen(key)
			call randombytes_buf(nonce,nb)
			ret=crypto_aead_aes256gcm_encrypt(ciphertext,clen,msg,mlen,ad,adlen,c_null_char,nonce,key)
			call check(error,ret,0,"encrypt")
			ret=crypto_aead_aes256gcm_decrypt(dmsg,dlen,c_null_char,ciphertext,clen,ad,adlen,nonce,key)
			call check(error,ret,0,"decrypt")
			print*,"here1"
			call check(error,msg,dmsg)
			print*,"here2"
			call check(error,mlen,dlen)
			if (allocated(error)) return
		endsubroutine test_aead_aes256gcm

endmodule tests_crypto_aead_aes256gcm
