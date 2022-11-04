module mod_crypto_secretbox_xsalsa20poly1305

	use,intrinsic::iso_c_binding
	! use::mod_common

	implicit none

	private

	public::crypto_secretbox_xsalsa20poly1305_keybytes
	public::crypto_secretbox_xsalsa20poly1305_noncebytes
	public::crypto_secretbox_xsalsa20poly1305_macbytes
	public::crypto_secretbox_xsalsa20poly1305_messagebytes_max
	public::crypto_secretbox_xsalsa20poly1305
	public::crypto_secretbox_xsalsa20poly1305_open
	public::crypto_secretbox_xsalsa20poly1305_keygen
	public::crypto_secretbox_xsalsa20poly1305_boxzerobytes
	public::crypto_secretbox_xsalsa20poly1305_zerobytes

	! #define crypto_secretbox_xsalsa20poly1305_KEYBYTES 32U
	! #define crypto_secretbox_xsalsa20poly1305_NONCEBYTES 24U
	! #define crypto_secretbox_xsalsa20poly1305_MACBYTES 16U
	! #define crypto_secretbox_xsalsa20poly1305_BOXZEROBYTES 16U

	interface

		function crypto_secretbox_xsalsa20poly1305_keybytes()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_keybytes

		function crypto_secretbox_xsalsa20poly1305_noncebytes()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_noncebytes

		function crypto_secretbox_xsalsa20poly1305_macbytes()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_macbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_macbytes

		function crypto_secretbox_xsalsa20poly1305_messagebytes_max()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_messagebytes_max

		function crypto_secretbox_xsalsa20poly1305(c,m,mlen,n,k)&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_secretbox_xsalsa20poly1305

		function crypto_secretbox_xsalsa20poly1305_open(m,c,clen,n,k)&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_open')&
		&result(res)
			import::c_char,c_int,c_long_long
		integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_secretbox_xsalsa20poly1305_open

		subroutine crypto_secretbox_xsalsa20poly1305_keygen(k)&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_secretbox_xsalsa20poly1305_keygen

		function crypto_secretbox_xsalsa20poly1305_boxzerobytes()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_boxzerobytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_boxzerobytes

		function crypto_secretbox_xsalsa20poly1305_zerobytes()&
		&bind(c,name='crypto_secretbox_xsalsa20poly1305_zerobytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretbox_xsalsa20poly1305_zerobytes

	endinterface

endmodule mod_crypto_secretbox_xsalsa20poly1305

