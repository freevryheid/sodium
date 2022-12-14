module tests_crypto_secretbox

	use, intrinsic :: iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_secretbox

	contains

		subroutine collect_tests_crypto_secretbox(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for key",test_key)]
		endsubroutine collect_tests_crypto_secretbox

		subroutine test_key(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::key,message,nonce,cipher,decrypted
			integer(kind=c_size_t)::kb,nb,mb
			integer::res,ml,cl
			kb=crypto_secretbox_keybytes()
			nb=crypto_secretbox_noncebytes()
			mb=crypto_secretbox_macbytes()
			allocate(character(len=kb)::key)
			allocate(character(len=nb)::nonce)
			call crypto_secretbox_keygen(key)
			call randombytes_buf(nonce,nb)
			message="test"
			ml=4
			cl=ml+mb
			allocate(character(len=cl)::cipher)
			allocate(character(len=ml)::decrypted)
			res=crypto_secretbox_easy(cipher,c_str(message),int(ml,kind=c_long_long),nonce,key)
			call check(error,res,0)
			res=crypto_secretbox_open_easy(decrypted,cipher,int(cl,kind=c_long_long),nonce,key)
			call check(error,res,0)
			call check(error,decrypted,message)
			if (allocated(error)) return
		endsubroutine test_key

endmodule tests_crypto_secretbox
