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
			m = "test" // c_null_char ! msg - c strings are null terminated
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
