module mod_crypto_core_salsa208

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_core_salsa208_outputbytes
	public::crypto_core_salsa208_inputbytes
	public::crypto_core_salsa208_keybytes
	public::crypto_core_salsa208_constbytes
	public::crypto_core_salsa208

 ! #define crypto_core_salsa208_OUTPUTBYTES 64U
 ! #define crypto_core_salsa208_INPUTBYTES 16U
 ! #define crypto_core_salsa208_KEYBYTES 32U
 ! #define crypto_core_salsa208_CONSTBYTES 16U

	interface

		function crypto_core_salsa208_outputbytes(void)bind(c,name='crypto_core_salsa208_outputbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_salsa208_outputbytes

		function crypto_core_salsa208_inputbytes(void)bind(c,name='crypto_core_salsa208_inputbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_salsa208_inputbytes

		function crypto_core_salsa208_keybytes(void)bind(c,name='crypto_core_salsa208_keybytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_salsa208_keybytes

		function crypto_core_salsa208_constbytes(void)bind(c,name='crypto_core_salsa208_constbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_salsa208_constbytes

		function crypto_core_salsa208(out,in,k,c)bind(c,name='crypto_core_salsa208')result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::out,in,k,c
		end function crypto_core_salsa208

	endinterface

endmodule mod_crypto_core_salsa208
