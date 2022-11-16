module tests_crypto_scalarmult

	use,intrinsic::iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_scalarmult

	contains

		subroutine collect_tests_crypto_scalarmult(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for scalarmult",test_scalarmult)]
		endsubroutine collect_tests_crypto_scalarmult

		subroutine test_scalarmult(error)
			type(error_type),allocatable,intent(out)::error
			character(len=32)::alicesk,bobsk,small_order_p
			character(len=:),allocatable::hex,res
			integer(kind=c_signed_char),dimension(32)::a,b,c
			integer(kind=c_signed_char)::z
			! type(c_ptr)::ap,bp,kp ! pointers to secure memory strings
			integer(kind=c_size_t)::siz,hexlen
			character(len=:),pointer::alicepk,bobpk,k
			integer::ret
			data a/z'77',z'07',z'6d',z'0a',z'73',z'18',z'a5',z'7d',&
						&z'3c',z'16',z'c1',z'72',z'51',z'b2',z'66',z'45',&
						&z'df',z'4c',z'2f',z'87',z'eb',z'c0',z'99',z'2a',&
						&z'b1',z'77',z'fb',z'a5',z'1d',z'b9',z'2c',z'2a'/
			data b/z'5d',z'ab',z'08',z'7e',z'62',z'4a',z'8a',z'4b',&
						&z'79',z'e1',z'7f',z'8b',z'83',z'80',z'0e',z'e6',&
						&z'6f',z'3b',z'b1',z'29',z'26',z'18',z'b6',z'fd',&
						&z'1c',z'2f',z'8b',z'27',z'ff',z'88',z'e0',z'eb'/
			data c/z'e0',z'eb',z'7a',z'7c',z'3b',z'41',z'b8',z'ae',&
						&z'16',z'56',z'e3',z'fa',z'f1',z'9f',z'c4',z'6a',&
						&z'da',z'09',z'8d',z'eb',z'9c',z'32',z'b1',z'fd',&
						&z'86',z'62',z'05',z'16',z'5f',z'49',z'b8',z'00'/
			data z/z'80'/
			siz=crypto_scalarmult_bytes()
			hexlen=siz*2+1
			allocate(character(len=hexlen)::hex)
			ret=sodium_init()
			call check(error,ret.ne.-1,"init")
			alicesk=transfer(a,alicesk)
			bobsk=transfer(b,bobsk)
			small_order_p=transfer(c,small_order_p)
			call sodium_malloc(alicepk,siz)
			call sodium_malloc(bobpk,siz)
			call sodium_malloc(k,siz)
			ret=crypto_scalarmult_base(alicepk,alicesk)
			call check(error,ret,0)
			res=sodium_bin2hex(hex,hexlen,alicepk,siz)
			call check(error,c_str(res),hex)
			print*,"hex:"//hex
			ret=crypto_scalarmult_base(bobpk,bobsk)
			call check(error,ret,0)
			res=sodium_bin2hex(hex,hexlen,bobpk,siz)
			call check(error,c_str(res),hex)
			print*,"hex:"//hex
			ret=crypto_scalarmult(k,alicesk,bobpk)
			call check(error,ret,0)
			res=sodium_bin2hex(hex,hexlen,k,siz)
			call check(error,c_str(res),hex)
			print*,"hex:"//hex
			ret=crypto_scalarmult(k,bobsk,alicepk)
			call check(error,ret,0)
			res=sodium_bin2hex(hex,hexlen,k,siz)
			call check(error,c_str(res),hex)
			print*,"hex:"//hex
			alicepk(32:32)=char(z)
			ret=crypto_scalarmult(k,bobsk,alicepk)
			call check(error,ret,0)
			res=sodium_bin2hex(hex,hexlen,k,siz)
			call check(error,c_str(res),hex)
			print*,"hex:"//hex
			ret=crypto_scalarmult(k,bobsk,small_order_p)
			call check(error,ret,-1)
			call sodium_free(alicepk)
			call sodium_free(bobpk)
			call sodium_free(k)
			call check(error,crypto_scalarmult_primitive(),"curve25519")
			if (allocated(error)) return
		endsubroutine test_scalarmult

endmodule tests_crypto_scalarmult
