module tests_crypto_pwhash

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_pwhash

	contains

		subroutine collect_tests_crypto_pwhash(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for pwhash",test_pwhash)]
		endsubroutine collect_tests_crypto_pwhash

		subroutine test_pwhash(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::pwd,salt,key,hashed_pwd
			integer(kind=c_size_t)::saltb,seedb,strb
			integer(kind=c_size_t)::klen,memlimit
			integer(kind=c_long_long)::pwdlen,opslimit
			integer::alg,res
			! key derivation
			pwd="Correct Horse Battery Staple"
			pwdlen=len(pwd)
			seedb=crypto_box_seedbytes()
			saltb=crypto_pwhash_saltbytes()
			opslimit=crypto_pwhash_opslimit_interactive()
			memlimit=crypto_pwhash_memlimit_interactive()
			alg=crypto_pwhash_alg_default()
			allocate(character(len=saltb)::salt)
			allocate(character(len=seedb)::key)
			call randombytes_buf(salt,saltb)
			res=crypto_pwhash(key,seedb,pwd,pwdlen,salt,opslimit,memlimit,alg)
			call check(error,res,0)
			! password storage
			strb=crypto_pwhash_strbytes()
			allocate(character(len=strb)::hashed_pwd)
			opslimit=crypto_pwhash_opslimit_sensitive()
			memlimit=crypto_pwhash_memlimit_sensitive()
			print*,"generating a hashed password - this may take some time to complete"
			res=crypto_pwhash_str(hashed_pwd,pwd,pwdlen,opslimit,memlimit)
			call check(error,res,0)
			res=crypto_pwhash_str_verify(hashed_pwd,pwd,pwdlen)
			call check(error,res,0)
			if (allocated(error)) return
		endsubroutine test_pwhash

endmodule tests_crypto_pwhash
