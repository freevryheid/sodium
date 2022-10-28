program main

	use ffi_utils
	use stdlib_string_type

	implicit none

	type(proc),dimension(:),allocatable::procs
	type(proc)::aproc
	type(param),dimension(:),allocatable::prms
	type(param)::aprm
	type(string_type)::st,cod,nam,typ,arr
	logical::ptr
	integer::i,j,prc

	procs=get_file()

	do i=1,size(procs)
		aproc=procs(i)
		nam=aproc%nam
		print*,"! public::"//nam
	enddo
	do i=1,size(procs)
		aproc=procs(i)
		nam=aproc%nam
		typ=aproc%typ
		prms=aproc%prms
		prc=aproc%prc
		cod=aproc%cod
		ptr=aproc%ptr
		st="! "//typ
		if(ptr) st=st//" (ptr)"
		print*,st
		selectcase(prc)
			case(FUN)
				print *,"! function "//nam//"("//params2string(prms)//")bind(c,name='"//nam//"')result(res)"
			case(SUB)
				print *,"! subroutine "//nam//"("//params2string(prms)//")bind(c,name='"//nam//"')"
			case(DEF)
				print*,"! "//cod
			case default
		endselect
		do j=1,size(prms)
			aprm=prms(j)
			nam=aprm%nam
			typ=aprm%typ
			st="! "//typ//" :: "//nam
			print*,st
		enddo
		print *,"!---"
	enddo

endprogram main
