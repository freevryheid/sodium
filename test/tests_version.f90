module tests_version

	use, intrinsic :: iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_version

	contains

		subroutine collect_tests_version(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[&
			&new_unittest("test for string",test_string),&
			&new_unittest("test for major",test_major),&
			&new_unittest("test for minor",test_minor),&
			&new_unittest("test for minimal",test_min)]
		endsubroutine collect_tests_version

		subroutine test_string(error)
			type(error_type),allocatable,intent(out)::error
			character(len=:),allocatable::res
			res=sodium_version_string()
			call check(error,res,"1.0.18")
			if (allocated(error)) return
		endsubroutine test_string

		subroutine test_major(error)
			type(error_type),allocatable,intent(out)::error
			call check(error,sodium_library_version_major(),10)
			if (allocated(error)) return
		endsubroutine test_major

		subroutine test_minor(error)
			type(error_type),allocatable,intent(out)::error
			call check(error,sodium_library_version_minor(),3)
			if (allocated(error)) return
		endsubroutine test_minor

		subroutine test_min(error)
			type(error_type),allocatable,intent(out)::error
			call check(error,sodium_library_minimal(),0)
			if (allocated(error)) return
		endsubroutine test_min

endmodule tests_version
