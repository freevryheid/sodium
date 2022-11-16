module term

	use, intrinsic :: iso_c_binding
	implicit none
	private
	public::c_setmode

	interface

		subroutine c_setmode(mode)bind(c,name='setmode')
			import :: c_int
			integer(kind=c_int)::mode
		endsubroutine c_setmode

	endinterface

endmodule term
