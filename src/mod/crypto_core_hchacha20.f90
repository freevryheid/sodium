module mod_crypto_core_hchacha20

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_core_hchacha20_outputbytes
	public::crypto_core_hchacha20_inputbytes
	public::crypto_core_hchacha20_keybytes
	public::crypto_core_hchacha20_constbytes
	public::crypto_core_hchacha20

	! #define crypto_core_hchacha20_OUTPUTBYTES 32U
	! #define crypto_core_hchacha20_INPUTBYTES 16U
	! #define crypto_core_hchacha20_KEYBYTES 32U
	! #define crypto_core_hchacha20_CONSTBYTES 16U

	interface

		function crypto_core_hchacha20_outputbytes()&
		&bind(c,name='crypto_core_hchacha20_outputbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_hchacha20_outputbytes

		function crypto_core_hchacha20_inputbytes()&
		&bind(c,name='crypto_core_hchacha20_inputbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_hchacha20_inputbytes

		function crypto_core_hchacha20_keybytes()&
		&bind(c,name='crypto_core_hchacha20_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_hchacha20_keybytes

		function crypto_core_hchacha20_constbytes()&
		&bind(c,name='crypto_core_hchacha20_constbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_core_hchacha20_constbytes

		function crypto_core_hchacha20(out,in,k,c)&
		&bind(c,name='crypto_core_hchacha20')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::out,in,k,c
		end function crypto_core_hchacha20

	endinterface

end module mod_crypto_core_hchacha20