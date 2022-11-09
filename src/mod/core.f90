module mod_core

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::sodium_init

	interface

		function sodium_init()&
		&bind(c,name='sodium_init')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction sodium_init

	endinterface

endmodule mod_core