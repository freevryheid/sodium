program main
	use stdlib_error
	use sodium
	implicit none
	if (sodium_init() < 0) call error_stop("panic: init failed - unsafe to use")
	print *, "safe"
end program main
