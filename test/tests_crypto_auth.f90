module tests_crypto_auth

	use, intrinsic :: iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_auth

	contains

		subroutine collect_tests_crypto_auth(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for verify",test_verify)]
		endsubroutine collect_tests_crypto_auth

		subroutine test_verify(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::key,mac,msg
			integer::klen,mlen,res
			integer(kind=c_long_long)::msg_len
			klen=crypto_auth_keybytes()
			mlen=crypto_auth_bytes()
			msg="test"
			msg_len=int(4,kind=c_long_long)
			allocate(character(len=klen)::key)
			allocate(character(len=mlen)::mac)
			call crypto_auth_keygen(key)
			res=crypto_auth(mac,msg,msg_len,key)
			call check(error,res,0)
			res=crypto_auth_verify(mac,msg,msg_len,key)
			call check(error,res,0)
			if (allocated(error)) return
		endsubroutine test_verify

endmodule tests_crypto_auth
