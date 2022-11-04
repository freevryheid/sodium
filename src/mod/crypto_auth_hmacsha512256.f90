module mod_crypto_auth_hmacsha512256

	use,intrinsic::iso_c_binding
	! use::mod_common
	use::mod_crypto_auth_hmacsha512

	implicit none

	private

	public::crypto_auth_hmacsha512256_bytes
	public::crypto_auth_hmacsha512256_keybytes
	public::crypto_auth_hmacsha512256
	public::crypto_auth_hmacsha512256_verify
	public::crypto_auth_hmacsha512256_statebytes
	public::crypto_auth_hmacsha512256_init
	public::crypto_auth_hmacsha512256_update
	public::crypto_auth_hmacsha512256_final
	public::crypto_auth_hmacsha512256_keygen
 !
 ! #define crypto_auth_hmacsha512256_BYTES 32U
 ! #define crypto_auth_hmacsha512256_KEYBYTES 32U

	interface

		function crypto_auth_hmacsha512256_bytes()&
		&bind(c,name='crypto_auth_hmacsha512256_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_auth_hmacsha512256_bytes

		function crypto_auth_hmacsha512256_keybytes()&
		&bind(c,name='crypto_auth_hmacsha512256_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_auth_hmacsha512256_keybytes

		function crypto_auth_hmacsha512256(out,in,inlen,k)&
		&bind(c,name='crypto_auth_hmacsha512256')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::k
		endfunction crypto_auth_hmacsha512256

		function crypto_auth_hmacsha512256_verify(h,in,inlen,k)&
		&bind(c,name='crypto_auth_hmacsha512256_verify')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::h
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::k
		endfunction crypto_auth_hmacsha512256_verify

		function crypto_auth_hmacsha512256_statebytes()&
		&bind(c,name='crypto_auth_hmacsha512256_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_auth_hmacsha512256_statebytes

		function crypto_auth_hmacsha512256_init(state,key,keylen)&
		&bind(c,name='crypto_auth_hmacsha512256_init')&
		&result(res)
			import::c_char,c_int,c_size_t,crypto_auth_hmacsha512_state
			integer(kind=c_int)::res
			type(crypto_auth_hmacsha512_state)::state
			character(kind=c_char)::key
			integer(kind=c_size_t),value::keylen
		endfunction crypto_auth_hmacsha512256_init

		function crypto_auth_hmacsha512256_update(state,in,inlen)&
		&bind(c,name='crypto_auth_hmacsha512256_update')&
		&result(res)
			import::c_int,crypto_auth_hmacsha512_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_auth_hmacsha512_state)::state
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
		endfunction crypto_auth_hmacsha512256_update

		function crypto_auth_hmacsha512256_final(state,out)&
		&bind(c,name='crypto_auth_hmacsha512256_final')&
		&result(res)
			import::c_int,c_char,crypto_auth_hmacsha512_state
			integer(kind=c_int)::res
			type(crypto_auth_hmacsha512_state)::state
			character(kind=c_char)::out
		endfunction crypto_auth_hmacsha512256_final

		subroutine crypto_auth_hmacsha512256_keygen(k)&
		&bind(c,name='crypto_auth_hmacsha512256_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_auth_hmacsha512256_keygen

	endinterface

endmodule mod_crypto_auth_hmacsha512256

