module mod_version

	use,intrinsic::iso_c_binding
	use mod_common

	implicit none

	private

	public::sodium_version_string
	public::sodium_library_version_major
	public::sodium_library_version_minor
	public::sodium_library_minimal

	interface

		function bind_sodium_version_string()&
		&bind(c,name='sodium_version_string')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_sodium_version_string

		function sodium_library_version_major()&
		&bind(c,name='sodium_library_version_major')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction sodium_library_version_major

		function sodium_library_version_minor()&
		&bind(c,name='sodium_library_version_minor')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction sodium_library_version_minor

		function sodium_library_minimal()&
		&bind(c,name='sodium_library_minimal')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction sodium_library_minimal

	endinterface

	contains

		function sodium_version_string()&
		&result(res)
			character(len=:),allocatable::res
			type(c_ptr)::cptr
			integer(kind=c_size_t)::siz
			cptr=bind_sodium_version_string()
			siz=c_strlen(cptr)
			allocate(character(len=siz)::res)
			call c_f_str_ptr(cptr,res)
		endfunction sodium_version_string

endmodule mod_version
