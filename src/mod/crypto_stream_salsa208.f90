module mod_crypto_stream_salsa208

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_stream_salsa208_keybytes
	public::crypto_stream_salsa208_noncebytes
	public::crypto_stream_salsa208_messagebytes_max
	public::crypto_stream_salsa208
	public::crypto_stream_salsa208_xor
	public::crypto_stream_salsa208_keygen

	! #define crypto_stream_salsa208_KEYBYTES 32U
	! #define crypto_stream_salsa208_NONCEBYTES 8U
	! #define crypto_stream_salsa208_MESSAGEBYTES_MAX SODIUM_SIZE_MAX

	interface

		function crypto_stream_salsa208_keybytes()&
		&bind(c,name='crypto_stream_salsa208_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_salsa208_keybytes

		function crypto_stream_salsa208_noncebytes()&
		&bind(c,name='crypto_stream_salsa208_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_salsa208_noncebytes

		function crypto_stream_salsa208_messagebytes_max()&
		&bind(c,name='crypto_stream_salsa208_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_salsa208_messagebytes_max

		function crypto_stream_salsa208(c,clen,n,k)&
		&bind(c,name='crypto_stream_salsa208')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_stream_salsa208

		function crypto_stream_salsa208_xor(c,m,mlen,n,k)&
		&bind(c,name='crypto_stream_salsa208_xor')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_stream_salsa208_xor

		subroutine crypto_stream_salsa208_keygen(k)&
		&bind(c,name='crypto_stream_salsa208_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_stream_salsa208_keygen

	endinterface

endmodule mod_crypto_stream_salsa208