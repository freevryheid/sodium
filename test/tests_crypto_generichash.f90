module tests_crypto_generichash

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive
	use stdlib_io

	implicit none

	private

	public :: collect_tests_crypto_generichash

	type tests
		character(len=:),allocatable::in_hex
		character(len=:),allocatable::key_hex
		character(len=:),allocatable::out_hex
	endtype

	contains

		subroutine collect_tests_crypto_generichash(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for generichash",test_generichash)]
		endsubroutine collect_tests_crypto_generichash

		subroutine test_generichash(error)
			type(error_type),allocatable,intent(out)::error
			type(tests)::test
			integer::u,iostat,ret,i
			character(len=:),pointer::key,out,expected_out,in
			character(len=:),allocatable::res
			! type(c_ptr)::p_key,p_out,p_expected_out,p_in
			integer(kind=c_size_t)::km,bm,hlen
			integer(kind=c_size_t),pointer::blen=>null()
			ret=sodium_init()
			call check(error,ret.ne.-1)
			km=crypto_generichash_blake2b_keybytes_max()
			bm=crypto_generichash_blake2b_bytes_max()
			call sodium_malloc(key,km)
			call sodium_malloc(out,bm)
			call sodium_malloc(expected_out,bm)
			u=open('test/tests_crypto_generichash.data',iostat=iostat)
			i=0
			do while(iostat.eq.0)
				call getline(u,test%in_hex,iostat)
				call getline(u,test%key_hex,iostat)
				call getline(u,test%out_hex,iostat)
				if(iostat.eq.0)then
					i=i+1
					! write(*,"(ai0)") "test #",i
					hlen=len(test%key_hex)
					ret=sodium_hex2bin(key,km,test%key_hex,hlen,c_null_char,blen,c_null_ptr)
					hlen=len(test%out_hex)
					ret=sodium_hex2bin(expected_out,bm,test%out_hex,hlen,c_null_char,blen,c_null_ptr)
					hlen=len(test%in_hex)
					call sodium_malloc(in,hlen)
					ret=sodium_hex2bin(in,hlen/2,test%in_hex,hlen,c_null_char,blen,c_null_ptr)
					ret=crypto_generichash(out,bm,in,hlen/2,key,km)
					call check(error,out,expected_out)
					call sodium_free(in)
				endif
			enddo
			close(u)
			call sodium_free(key)
			call sodium_free(out)
			call sodium_free(expected_out)
			if (allocated(error)) return
		endsubroutine test_generichash

endmodule tests_crypto_generichash
