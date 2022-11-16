program pwd

	use,intrinsic::iso_c_binding
	use::term
	use::sodium

	implicit none

	character(len=32)::input
	character(len=:),pointer::pw
	! type(c_ptr)::ppw
	integer(kind=c_size_t)::siz
	integer::ret

	call c_setmode(1)
	write(*,'(a)',advance='no')"password?: "
	read(*,'(a)')input
	call c_setmode(0)

	ret=sodium_init()
	if(ret.eq.-1)errorstop "cannot init sodium"
	siz=len_trim(input)
	call sodium_malloc(pw,siz)
	pw=trim(input)
	print*,new_line('a'),"you entered: "//pw
	call sodium_free(pw)

endprogram pwd
