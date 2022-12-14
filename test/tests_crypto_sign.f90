module tests_crypto_sign

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public::collect_tests_crypto_sign

	contains

		subroutine collect_tests_crypto_sign(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for sign",test_sign)]
		endsubroutine collect_tests_crypto_sign

		subroutine test_sign(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::pk,sk,msg,signed_message,unsigned_message,sig,m1,m2,seed,seed1,pk1
			integer(kind=c_size_t)::pb,sb,b,sbb
			integer(kind=c_long_long),pointer::signed_message_len,unsigned_message_len,siglen
			integer(kind=c_long_long)::mlen,m1l,m2l
			type(crypto_sign_state)::state
			integer::res
			pb=crypto_sign_publickeybytes()
			sb=crypto_sign_secretkeybytes()
			b=crypto_sign_bytes()
			msg="hello"
			mlen=5
			allocate(character(len=pb)::pk)
			allocate(character(len=sb)::sk)
			allocate(character(len=b+mlen)::signed_message)
			allocate(character(len=mlen)::unsigned_message)
			allocate(character(len=b)::sig)
			allocate(signed_message_len)
			allocate(unsigned_message_len)
			! combined mode
			res=crypto_sign_keypair(pk,sk)
			call check(error,res,0)
			res=crypto_sign(signed_message,signed_message_len,msg,mlen,sk)
			call check(error,res,0)
			res=crypto_sign_open(unsigned_message,unsigned_message_len,signed_message,signed_message_len,pk)
			call check(error,res,0)
			call check(error,unsigned_message,msg)
			call check(error,unsigned_message_len,mlen)
			! detached mode
			res=crypto_sign_keypair(pk,sk)
			call check(error,res,0)
			allocate(siglen)
			res=crypto_sign_detached(sig,siglen,msg,mlen,sk)
			call check(error,res,0)
			res=crypto_sign_verify_detached(sig,msg,mlen,pk)
			call check(error,res,0)
			! multi-part message
			m1="Arbitrary data to hash"
			m1l=len(m1)
			m2="is longer than expected"
			m2l=len(m2)
			! + signature creation
			res=crypto_sign_init(state)
			call check(error,res,0)
			res=crypto_sign_update(state,m1,m1l)
			call check(error,res,0)
			res=crypto_sign_update(state,m2,m2l)
			call check(error,res,0)
			res=crypto_sign_final_create(state,sig,siglen,sk)
			call check(error,res,0)
			! + signature verification
			res=crypto_sign_init(state)
			call check(error,res,0)
			res=crypto_sign_update(state,m1,m1l)
			call check(error,res,0)
			res=crypto_sign_update(state,m2,m2l)
			call check(error,res,0)
			res=crypto_sign_final_verify(state,sig,pk)
			call check(error,res,0)
			! extract seend and pk
			sbb=crypto_sign_seedbytes()
			allocate(character(len=sbb)::seed)
			allocate(character(len=sbb)::seed1)
			allocate(character(len=pb)::pk1)
			call randombytes_buf(seed,sbb)
			res=crypto_sign_seed_keypair(pk,sk,seed)
			call check(error,res,0)
			res=crypto_sign_ed25519_sk_to_seed(seed1,sk)
			call check(error,res,0)
			call check(error,seed,seed1)
			res=crypto_sign_ed25519_sk_to_pk(pk1,sk)
			call check(error,res,0)
			call check(error,pk,pk1)
			if (allocated(error)) return
		endsubroutine test_sign

endmodule tests_crypto_sign
