module mod_crypto_stream_chacha20

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_stream_chacha20_keybytes
	public::crypto_stream_chacha20_noncebytes
	public::crypto_stream_chacha20_messagebytes_max
	public::crypto_stream_chacha20
	public::crypto_stream_chacha20_xor
	public::crypto_stream_chacha20_xor_ic
	public::crypto_stream_chacha20_keygen
	public::crypto_stream_chacha20_ietf_keybytes
	public::crypto_stream_chacha20_ietf_noncebytes
	public::crypto_stream_chacha20_ietf_messagebytes_max
	public::crypto_stream_chacha20_ietf
	public::crypto_stream_chacha20_ietf_xor
	public::crypto_stream_chacha20_ietf_xor_ic
	public::crypto_stream_chacha20_ietf_keygen

	! #define crypto_stream_chacha20_KEYBYTES 32U
	! #define crypto_stream_chacha20_NONCEBYTES 8U
	! #define crypto_stream_chacha20_MESSAGEBYTES_MAX SODIUM_SIZE_MAX
	! #define crypto_stream_chacha20_ietf_KEYBYTES 32U
	! #define crypto_stream_chacha20_IETF_KEYBYTES crypto_stream_chacha20_ietf_KEYBYTES
	! #define crypto_stream_chacha20_IETF_NONCEBYTES crypto_stream_chacha20_ietf_NONCEBYTES
	! #define crypto_stream_chacha20_IETF_MESSAGEBYTES_MAX crypto_stream_chacha20_ietf_MESSAGEBYTES_MAX

	interface

		function crypto_stream_chacha20_keybytes()&
		&bind(c,name='crypto_stream_chacha20_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_keybytes

		function crypto_stream_chacha20_noncebytes()&
		&bind(c,name='crypto_stream_chacha20_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_noncebytes

		function crypto_stream_chacha20_messagebytes_max()&
		&bind(c,name='crypto_stream_chacha20_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_messagebytes_max

		function crypto_stream_chacha20(c,clen,n,k)&
		&bind(c,name='crypto_stream_chacha20')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_stream_chacha20

		function crypto_stream_chacha20_xor(c,m,mlen,n,k)&
		&bind(c,name='crypto_stream_chacha20_xor')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_stream_chacha20_xor

		function crypto_stream_chacha20_xor_ic(c,m,mlen,n,ic,k)&
		&bind(c,name='crypto_stream_chacha20_xor_ic')&
		&result(res)
			import::c_char,c_int,c_long_long,c_int64_t
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
			integer(kind=c_int64_t),value::ic
		endfunction crypto_stream_chacha20_xor_ic

		subroutine crypto_stream_chacha20_keygen(k)&
		&bind(c,name='crypto_stream_chacha20_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_stream_chacha20_keygen

		function crypto_stream_chacha20_ietf_keybytes()&
		&bind(c,name='crypto_stream_chacha20_ietf_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_ietf_keybytes

		function crypto_stream_chacha20_ietf_noncebytes()&
		&bind(c,name='crypto_stream_chacha20_ietf_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_ietf_noncebytes

		function crypto_stream_chacha20_ietf_messagebytes_max()&
		&bind(c,name='crypto_stream_chacha20_ietf_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_stream_chacha20_ietf_messagebytes_max

		function crypto_stream_chacha20_ietf(c,clen,n,k)&
		&bind(c,name='crypto_stream_chacha20_ietf')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_stream_chacha20_ietf

		function crypto_stream_chacha20_ietf_xor(c,m,mlen,n,k)&
		&bind(c,name='crypto_stream_chacha20_ietf_xor')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_stream_chacha20_ietf_xor

		function crypto_stream_chacha20_ietf_xor_ic(c,m,mlen,n,ic,k)&
		&bind(c,name='crypto_stream_chacha20_ietf_xor_ic')&
		&result(res)
			import::c_char,c_int,c_long_long,c_int64_t
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
			integer(kind=c_int64_t),value::ic
		endfunction crypto_stream_chacha20_ietf_xor_ic

		subroutine crypto_stream_chacha20_ietf_keygen(k)&
		&bind(c,name='crypto_stream_chacha20_ietf_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_stream_chacha20_ietf_keygen

	 endinterface

endmodule mod_crypto_stream_chacha20

