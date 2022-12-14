module tests_utils

	use,intrinsic::iso_c_binding
	use stdlib_kinds
	use sodium
	use testdrive

	implicit none

	private

	public::collect_tests_utils

	contains

		subroutine collect_tests_utils(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[&
			&new_unittest("test for init",test_init),&
			&new_unittest("test for bin2hex",test_bin2hex),&
			&new_unittest("test for hex2bin",test_hex2bin),&
			&new_unittest("test for bin2base64",test_bin2base64),&
			&new_unittest("test for base642bin",test_base642bin),&
			&new_unittest("test for increment",test_increment),&
			&new_unittest("test for add",test_add),&
			&new_unittest("test for sub",test_sub),&
			&new_unittest("test for compare",test_compare),&
			&new_unittest("test for is_zero",test_is_zero),&
			&new_unittest("test for stackzero",test_stackzero),&
			&new_unittest("test for padding",test_padding),&
			&new_unittest("test for memzero",test_memzero),&
			&new_unittest("test for malloc",test_malloc)&
			&]
		endsubroutine collect_tests_utils

		subroutine test_init(error)
			type(error_type),allocatable,intent(out)::error
			integer::r
			r=sodium_init()
			call check(error,r,0)
			if(allocated(error))return
		end subroutine test_init

		subroutine test_bin2hex(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::bin,hex,res
			integer(kind=c_size_t)::hex_maxlen,bin_len
			bin="abcdefghij"
			bin_len=len(bin)
			hex_maxlen=bin_len*2+1
			allocate(character(len=hex_maxlen)::hex)
			allocate(character(len=hex_maxlen)::res)
			res=sodium_bin2hex(hex,hex_maxlen,bin,bin_len)
			print*,"bin:",bin
			print*,"hex:",hex
			call check(error,c_str(res),hex)
			if(allocated(error))return
		endsubroutine test_bin2hex

		! ! TODO: experiment with different hexes
		subroutine test_hex2bin(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::bin,hex,ignore
			integer(kind=c_size_t)::bin_maxlen,hex_len,bin_len
			type(c_ptr)::hex_end
			integer::res
			hex="6162636465666768696a"
			hex_len=len(hex)
			ignore=c_null_char ! disallow any non-hexadecimal character
			hex_end=c_null_ptr
			bin_maxlen=hex_len/2+1
			bin_len=bin_maxlen
			print*,"binlen before:",bin_len
			allocate(character(len=bin_maxlen)::bin)
			res=sodium_hex2bin(bin,bin_maxlen,hex,hex_len,ignore,bin_len,hex_end)
			print*,"hex:",hex,"---"
			print*,"bin:",bin,"---"
			print*,"binlen after:",bin_len
			call check(error,res,0)
			if(allocated(error))return
		endsubroutine test_hex2bin

		subroutine test_bin2base64(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::bin,b64,res
			integer(kind=c_size_t)::b64_maxlen,bin_len
			integer::variant
			bin="11"
			bin_len=len(bin)
			variant=SODIUM_BASE64_VARIANT_ORIGINAL
			b64_maxlen=sodium_base64_encoded_len(bin_len,variant)
			allocate(character(len=b64_maxlen)::b64)
			allocate(character(len=b64_maxlen)::res)
			res=sodium_bin2base64(b64,b64_maxlen,bin,bin_len,variant)
			call check(error,c_str(res),b64)
			if(allocated(error))return
		endsubroutine test_bin2base64

		subroutine test_base642bin(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::bin,b64,ignore
			integer(kind=c_size_t)::bin_maxlen,b64_len,bin_len
			type(c_ptr)::b64_end
			integer::res, variant
			variant=SODIUM_BASE64_VARIANT_ORIGINAL
			b64="1010"
			! b64 = "VGhpcyBpcyBhIGpvdXJu" // "\n" // "ZXkgaW50by" // " " // "Bzb3VuZA=="
			b64_len=len(b64)
			ignore=c_null_char ! disallow any non-b64adecimal character
			b64_end=c_null_ptr
			bin_maxlen=b64_len/4*3
			bin_len=bin_maxlen
			allocate(character(len=bin_maxlen)::bin)
			res = sodium_base642bin(bin,bin_maxlen,b64,b64_len,ignore,bin_len,b64_end,variant)
			call check(error,res,0)
			if(allocated(error))return
		endsubroutine test_base642bin

		subroutine test_increment(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: n
			integer(kind=c_size_t) :: nlen
			n = "111"
			nlen = len(n)
			call sodium_increment(n, nlen)
			call check(error, n, "211")
			if (allocated(error)) return
		end subroutine test_increment

		subroutine test_add(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: a, b
			integer(kind=c_size_t) :: nlen
			a = "111"
			b = "111"
			nlen = len(a)
			call sodium_add(a, b, nlen)
			call check(error, a, "bbb")
			if (allocated(error)) return
		end subroutine test_add

		! FIXME
		subroutine test_sub(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: a, b
			integer(kind=c_size_t) :: nlen
			a = "1"
			b = "A"
			nlen = len(a)
			call sodium_sub(a, b, nlen)
			print *, a
			! call check(error, a, "bbb")
			! if (allocated(error)) return
		end subroutine test_sub

		subroutine test_compare(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: b1, b2
			integer(kind=c_size_t) :: blen
			integer :: res
			b1 = "11"
			b2 = b1
			blen = len(b1)
			res = sodium_compare(b1, b2, blen)
			call check(error, res, 0)
			if (allocated(error)) return
			b2 = "10"
			res = sodium_compare(b1, b2, blen)
			call check(error, res, 1)
			if (allocated(error)) return
			res = sodium_compare(b2, b1, blen)
			call check(error, res, -1)
			if (allocated(error)) return
		end subroutine test_compare

		subroutine test_is_zero(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: n
			integer(kind=c_size_t) :: nlen
			integer :: res
			n = "11"
			nlen = len(n)
			res = sodium_is_zero(n, nlen)
			call check(error, res, 0)
			if (allocated(error)) return
		end subroutine test_is_zero

		! TODO: how to test?
		subroutine test_stackzero(error)
			type(error_type), allocatable, intent(out) :: error
			integer(kind=c_size_t) :: nlen
			nlen = 100
			call sodium_stackzero(nlen)
			! call check(error, res, 0)
			! if (allocated(error)) return
		end subroutine test_stackzero

		subroutine test_padding(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: buf
			integer(kind=c_size_t) :: padded_buflen, unpadded_buflen, blocksize, max_buflen
			integer :: res
			unpadded_buflen = 10
			blocksize = 16
			max_buflen = 100
			allocate(character(len=max_buflen) :: buf)
			res = sodium_pad(padded_buflen, buf, unpadded_buflen, blocksize, max_buflen)
			call check(error, res, 0)
			if (allocated(error)) return
			res = sodium_unpad(unpadded_buflen, buf, padded_buflen, blocksize)
			call check(error, res, 0)
			if (allocated(error)) return
		end subroutine test_padding

		subroutine test_memzero(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: buf
			integer(kind=c_size_t) :: buflen
			integer :: res
			buf = "hello"
			buflen = len(buf)
			call sodium_memzero(buf, buflen)
			res = sodium_is_zero(buf, buflen)
			call check(error, res, 1)
			if (allocated(error)) return
		end subroutine test_memzero

		subroutine test_malloc(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), pointer :: buf
			integer(kind=c_size_t) :: siz
			siz = 100
			call sodium_malloc(buf,siz)
			buf = "AAA"
			print *, buf
			! call check(error, res, 1)
			if (allocated(error)) return
		end subroutine test_malloc

end module tests_utils
