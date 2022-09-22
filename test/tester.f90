program tester
	use, intrinsic :: iso_fortran_env, only : error_unit
	use testdrive, only : run_testsuite!, color_tests
	use tests_sodium, only : collect_tests_sodium

	implicit none
	integer :: stat
	character(len=:), allocatable :: test

	! color_tests = .true.

	test = "1234"

	if (scan(test, "1") > 0) then
		stat = 0
		print *, new_line('a')," ... sodium tests ... "
		call run_testsuite(collect_tests_sodium, error_unit, stat)
		if (stat > 0) then
			write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
			error stop
		end if
	end if

end program tester
