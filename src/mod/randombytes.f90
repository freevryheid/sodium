module mod_randombytes

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::randombytes_seedbytes
	public::randombytes_buf
	public::randombytes_buf_deterministic
	public::randombytes_random
	public::randombytes_uniform
	public::randombytes_stir
	public::randombytes_close
	! public::randombytes_set_implementation
	! public::randombytes_implementation_name
	public::randombytes

	! #define randombytes_BYTES_MAX SODIUM_MIN(SODIUM_SIZE_MAX,0xffffffffUL)
	! #define randombytes_SEEDBYTES 32U

	interface

		function randombytes_seedbytes()&
		&bind(c,name='randombytes_seedbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction randombytes_seedbytes

		subroutine randombytes_buf(buf,size)&
		&bind(c,name='randombytes_buf')
			import::c_char,c_size_t
			character(kind=c_char)::buf
			integer(kind=c_size_t),value::size
		endsubroutine randombytes_buf

		subroutine randombytes_buf_deterministic(buf,size,seed)&
		&bind(c,name='randombytes_buf_deterministic')
			import::c_char,c_size_t
			character(kind=c_char)::buf
			integer(kind=c_size_t),value::size
			character(kind=c_char)::seed
		endsubroutine randombytes_buf_deterministic

		function randombytes_random()&
		&bind(c,name='randombytes_random')&
		&result(res)
			import::c_int64_t
			integer(kind=c_int64_t)::res
		endfunction randombytes_random

		function randombytes_uniform(upper_bound)&
		&bind(c,name='randombytes_uniform')&
		&result(res)
			import::c_int64_t
			integer(kind=c_int64_t),value::upper_bound
			integer(kind=c_int64_t)::res
		endfunction randombytes_uniform

		subroutine randombytes_stir()&
		&bind(c,name='randombytes_stir')
		endsubroutine randombytes_stir

		function randombytes_close()&
		&bind(c,name='randombytes_close')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction randombytes_close

		!---
		! int
		! function randombytes_set_implementation(impl)bind(c,name='randombytes_set_implementation')result(res)
		! randombytes_implementation :: impl
		!---
		! const char (ptr)
		! function randombytes_implementation_name(void)bind(c,name='randombytes_implementation_name')result(res)
		!---

		subroutine randombytes(buf,buf_len)&
		&bind(c,name='randombytes')
			import::c_char,c_long_long
			character(kind=c_char)::buf
			integer(kind=c_long_long),value::buf_len
		endsubroutine randombytes

	endinterface

endmodule mod_randombytes