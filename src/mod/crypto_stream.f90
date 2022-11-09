module mod_crypto_stream

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_stream_keybytes
	public::crypto_stream_noncebytes
	public::crypto_stream_messagebytes_max
	public::crypto_stream_primitive
	public::crypto_stream
	public::crypto_stream_xor
	public::crypto_stream_keygen

	! #define crypto_stream_KEYBYTES crypto_stream_xsalsa20_KEYBYTES
	! #define crypto_stream_NONCEBYTES crypto_stream_xsalsa20_NONCEBYTES
	! #define crypto_stream_MESSAGEBYTES_MAX crypto_stream_xsalsa20_MESSAGEBYTES_MAX
	! #define crypto_stream_PRIMITIVE "xsalsa20"

	interface

		function crypto_stream_keybytes()&
		&bind(c,name='crypto_stream_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_keybytes

		function crypto_stream_noncebytes()&
		&bind(c,name='crypto_stream_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_noncebytes

		function crypto_stream_messagebytes_max()&
		&bind(c,name='crypto_stream_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_messagebytes_max

		function bind_crypto_stream_primitive()&
		&bind(c,name='crypto_stream_primitive')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_stream_primitive

		function crypto_stream(c,clen,n,k)&
		&bind(c,name='crypto_stream')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_stream

		function crypto_stream_xor(c,m,mlen,n,k)&
		&bind(c,name='crypto_stream_xor')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_stream_xor

		subroutine crypto_stream_keygen(k)&
		&bind(c,name='crypto_stream_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_stream_keygen

	endinterface

	contains

		function crypto_stream_primitive()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_stream_primitive()
			call c_f_str_ptr(res1,res)
		end function crypto_stream_primitive

endmodule mod_crypto_stream