module mod_crypto_core_hsalsa20

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_core_hsalsa20_outputbytes
	public::crypto_core_hsalsa20_inputbytes
	public::crypto_core_hsalsa20_keybytes
	public::crypto_core_hsalsa20_constbytes
	public::crypto_core_hsalsa20

	! #define crypto_core_hsalsa20_OUTPUTBYTES 32U
	! #define crypto_core_hsalsa20_INPUTBYTES 16U
	! #define crypto_core_hsalsa20_KEYBYTES 32U
	! #define crypto_core_hsalsa20_CONSTBYTES 16U

	interface

		function crypto_core_hsalsa20_outputbytes()&
		&bind(c,name='crypto_core_hsalsa20_outputbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_core_hsalsa20_outputbytes

		function crypto_core_hsalsa20_inputbytes()&
		&bind(c,name='crypto_core_hsalsa20_inputbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_core_hsalsa20_inputbytes

		function crypto_core_hsalsa20_keybytes()&
		&bind(c,name='crypto_core_hsalsa20_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_core_hsalsa20_keybytes

		function crypto_core_hsalsa20_constbytes()&
		&bind(c,name='crypto_core_hsalsa20_constbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_core_hsalsa20_constbytes

		function crypto_core_hsalsa20(out,in,k,c)&
		&bind(c,name='crypto_core_hsalsa20')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::out,in,k,c
		endfunction crypto_core_hsalsa20

	endinterface

endmodule mod_crypto_core_hsalsa20
