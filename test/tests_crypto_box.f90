module tests_crypto_box

	use, intrinsic :: iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_box

	contains

		subroutine collect_tests_crypto_box(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for keypair",test_keypair)]
		endsubroutine collect_tests_crypto_box

		subroutine test_keypair(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::pk,sk,seed,message,nonce,cipher,decrypted
			character(len=:),allocatable::alice_pk,alice_sk,bob_pk,bob_sk
			integer(kind=c_size_t)::pkb,skb,sb,nb,mb
			integer::res,ml,cl
			pkb=crypto_box_publickeybytes()
			skb=crypto_box_secretkeybytes()
			sb=crypto_box_seedbytes()
			nb=crypto_box_noncebytes()
			mb=crypto_box_macbytes()
			allocate(character(len=pkb)::pk)
			allocate(character(len=skb)::sk)
			allocate(character(len=pkb)::alice_pk)
			allocate(character(len=skb)::alice_sk)
			allocate(character(len=pkb)::bob_pk)
			allocate(character(len=skb)::bob_sk)
			allocate(character(len=sb)::seed)
			allocate(character(len=nb)::nonce)
			call randombytes_buf(seed,sb)
			call randombytes_buf(nonce,nb)
			res=crypto_box_seed_keypair(pk,sk,seed)
			call check(error,res,0)
			res=crypto_box_keypair(alice_pk,alice_sk)
			call check(error,res,0)
			res=crypto_box_keypair(bob_pk,bob_sk)
			call check(error,res,0)
			message="test"
			ml=4
			cl=ml+mb
			allocate(character(len=cl)::cipher)
			allocate(character(len=ml)::decrypted)
			res=crypto_box_easy(cipher,c_str(message),int(ml,kind=c_long_long),nonce,bob_pk,alice_sk) ! note this wants a c-string
			call check(error,res,0)
			res=crypto_box_open_easy(decrypted,cipher,int(cl,kind=c_long_long),nonce,alice_pk,bob_sk)
			call check(error,res,0)
			call check(error,decrypted,message)
			res=crypto_scalarmult_base(alice_pk,bob_sk) ! retrieve public key from secret key previously generated using crypto_key_pair
			call check(error,res,0)
			call check(error,bob_pk,alice_pk)
			if (allocated(error)) return
		endsubroutine test_keypair

endmodule tests_crypto_box
