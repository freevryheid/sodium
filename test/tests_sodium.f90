module tests_sodium

	use, intrinsic :: iso_c_binding
	use stdlib_kinds
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_sodium

	contains

		subroutine collect_tests_sodium(testsuite)
			type(unittest_type), allocatable, intent(out) :: testsuite(:)
			testsuite = [new_unittest("test for init", test_init), &
			new_unittest("test for random", test_random), &
			new_unittest("test for uniform", test_uniform), &
			new_unittest("test for buf", test_buf), &
			new_unittest("test for bin2hex", test_bin2hex), &
			new_unittest("test for hex2bin", test_hex2bin), &
			new_unittest("test for bin2base64", test_bin2base64), &
			new_unittest("test for base642bin", test_base642bin), &
			new_unittest("test for increment", test_increment), &
			new_unittest("test for add", test_add), &
			new_unittest("test for sub", test_sub), &
			new_unittest("test for compare", test_compare), &
			new_unittest("test for is_zero", test_is_zero), &
			new_unittest("test for stackzero", test_stackzero), &
			new_unittest("test for secretbox_keygen", test_key), &
			new_unittest("test for secretbox_easy", test_easy)]
		end subroutine collect_tests_sodium

		subroutine test_init(error)
			type(error_type), allocatable, intent(out) :: error
			integer :: r
			r = sodium_init()
			call check(error, r, 0)
			if (allocated(error)) return
		end subroutine test_init

		subroutine test_random(error)
			type(error_type), allocatable, intent(out) :: error
			integer(kind=int64) :: i, r
			do i = 1, 1000000
				r = randombytes_random()
				call check(error, (r.ge.0_int64.and.r.le.4294967295_int64))
				if (allocated(error)) return
			end do
		end subroutine test_random

		subroutine test_uniform(error)
			type(error_type), allocatable, intent(out) :: error
			integer(kind=int64) :: i, r, upper
			upper = 100
			do i = 1, 1000000
				r = randombytes_uniform(upper)
				call check(error, (r.ge.0_int64.and.r.lt.upper))
				if (allocated(error)) return
			end do
		end subroutine test_uniform

		subroutine test_buf(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: buf1, buf2
			integer(kind=c_size_t) :: siz ! this will be int32 or int64 depending on arch
			integer :: r
			siz = 10
			allocate(character(len=siz) :: buf1)
			call randombytes_buf(buf1, siz)
			buf2 = buf1
			r = sodium_memcmp(buf1, buf2, siz)
			call check(error, r, 0)
			if (allocated(error)) return
		end subroutine test_buf

		subroutine test_bin2hex(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: bin, hex, res
			integer(kind=c_size_t) :: hex_maxlen, bin_len
			bin = "11"
			bin_len = len(bin)
			hex_maxlen = bin_len*2 + 1
			allocate(character(len=hex_maxlen) :: hex)
			allocate(character(len=hex_maxlen) :: res)
			res = sodium_bin2hex(hex, hex_maxlen, bin, bin_len)
			call check(error, c_str(res), hex)
			if (allocated(error)) return
		end subroutine test_bin2hex

		! TODO: experiment with different hexes
		subroutine test_hex2bin(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: bin, hex, ignore
			integer(kind=c_size_t) :: bin_maxlen, hex_len, bin_len
			type(c_ptr) :: hex_end
			integer :: res
			hex = "3131"
			hex_len = len(hex)
			ignore = c_null_char ! disallow any non-hexadecimal character
			hex_end = c_null_ptr
			bin_maxlen = 2
			bin_len = bin_maxlen
			allocate(character(len=bin_maxlen) :: bin)
			res = sodium_hex2bin(bin, bin_maxlen, hex, hex_len, ignore, bin_len, hex_end)
			call check(error, res, 0)
			if (allocated(error)) return
		end subroutine test_hex2bin

		subroutine test_bin2base64(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: bin, b64, res
			integer(kind=c_size_t) :: b64_maxlen, bin_len
			integer :: variant
			bin = "11"
			bin_len = len(bin)
			variant = SODIUM_BASE64_VARIANT_ORIGINAL
			b64_maxlen = sodium_base64_encoded_len(bin_len, variant)
			allocate(character(len=b64_maxlen) :: b64)
			allocate(character(len=b64_maxlen) :: res)
			res = sodium_bin2base64(b64, b64_maxlen, bin, bin_len, variant)
			call check(error, c_str(res), b64)
			if (allocated(error)) return
		end subroutine test_bin2base64

		subroutine test_base642bin(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: bin, b64, ignore
			integer(kind=c_size_t) :: bin_maxlen, b64_len, bin_len
			type(c_ptr) :: b64_end
			integer :: res, variant
			variant = SODIUM_BASE64_VARIANT_ORIGINAL
			b64 = "1010"
			! b64 = "VGhpcyBpcyBhIGpvdXJu" // "\n" // "ZXkgaW50by" // " " // "Bzb3VuZA=="
			b64_len = len(b64)
			ignore = c_null_char ! disallow any non-b64adecimal character
			b64_end = c_null_ptr
			bin_maxlen = b64_len/4*3
			bin_len = bin_maxlen
			allocate(character(len=bin_maxlen) :: bin)
			res = sodium_base642bin(bin, bin_maxlen, b64, b64_len, ignore, bin_len, b64_end, variant)
			call check(error, res, 0)
			if (allocated(error)) return
		end subroutine test_base642bin

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

		subroutine test_key(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: key
			integer :: i, k
			key = crypto_secretbox_keygen()
			k = crypto_secretbox_keybytes()
			call check(error, len(key), k)
			if (allocated(error)) return
		end subroutine test_key

		subroutine test_easy(error)
			type(error_type), allocatable, intent(out) :: error
			character(len=:), allocatable :: c, m, n, k
			integer(kind=int64) :: mlen, clen
			integer :: r, i
			k = crypto_secretbox_keygen() ! key
			allocate(character(len=crypto_secretbox_noncebytes()) :: n) ! nonce
			call randombytes_buf(n, crypto_secretbox_noncebytes())
			m = c_str("test") ! msg - c strings are null terminated
			mlen = len(m) - 1 ! length without trailing null
			clen = mlen + crypto_secretbox_macbytes()
			allocate(character(len=clen) :: c)
			r = crypto_secretbox_easy(c, m, mlen, n, k)
			call check(error, r, 0)
			if (allocated(error)) return
			r = crypto_secretbox_open_easy(m, c, clen, n, k)
			call check(error, r, 0)
			if (allocated(error)) return
		end subroutine test_easy

end module tests_sodium
