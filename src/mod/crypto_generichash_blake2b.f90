module mod_crypto_generichash_blake2b

	use,intrinsic::iso_c_binding

	implicit none

	private

	public::crypto_generichash_blake2b_bytes_min
	public::crypto_generichash_blake2b_bytes_max
	public::crypto_generichash_blake2b_bytes
	public::crypto_generichash_blake2b_keybytes_min
	public::crypto_generichash_blake2b_keybytes_max
	public::crypto_generichash_blake2b_keybytes
	public::crypto_generichash_blake2b_saltbytes
	public::crypto_generichash_blake2b_personalbytes
	public::crypto_generichash_blake2b_statebytes
	public::crypto_generichash_blake2b
	public::crypto_generichash_blake2b_salt_personal
	public::crypto_generichash_blake2b_init
	public::crypto_generichash_blake2b_init_salt_personal
	public::crypto_generichash_blake2b_update
	public::crypto_generichash_blake2b_final
	public::crypto_generichash_blake2b_keygen

	! #define crypto_generichash_blake2b_BYTES_MIN 16U
	! #define crypto_generichash_blake2b_BYTES_MAX 64U
	! #define crypto_generichash_blake2b_BYTES 32U
	! #define crypto_generichash_blake2b_KEYBYTES_MIN 16U
	! #define crypto_generichash_blake2b_KEYBYTES_MAX 64U
	! #define crypto_generichash_blake2b_KEYBYTES 32U
	! #define crypto_generichash_blake2b_SALTBYTES 16U
	! #define crypto_generichash_blake2b_PERSONALBYTES 16U

	type,bind(c)::blake2b
		character(kind=c_char)::opaque(384)
	endtype

	type,public,bind(c)::crypto_generichash_blake2b_state
		type(blake2b)::crypto_align(64)
	endtype

	interface

		function crypto_generichash_blake2b_bytes_min()&
		&bind(c,name='crypto_generichash_blake2b_bytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_bytes_min

		function crypto_generichash_blake2b_bytes_max()&
		&bind(c,name='crypto_generichash_blake2b_bytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_bytes_max

		function crypto_generichash_blake2b_bytes()&
		&bind(c,name='crypto_generichash_blake2b_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_bytes

		function crypto_generichash_blake2b_keybytes_min()&
		&bind(c,name='crypto_generichash_blake2b_keybytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_keybytes_min

		function crypto_generichash_blake2b_keybytes_max()&
		&bind(c,name='crypto_generichash_blake2b_keybytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_keybytes_max

		function crypto_generichash_blake2b_keybytes()&
		&bind(c,name='crypto_generichash_blake2b_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_keybytes

		function crypto_generichash_blake2b_saltbytes()&
		&bind(c,name='crypto_generichash_blake2b_saltbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_saltbytes

		function crypto_generichash_blake2b_personalbytes()&
		&bind(c,name='crypto_generichash_blake2b_personalbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_personalbytes

		function crypto_generichash_blake2b_statebytes()&
		&bind(c,name='crypto_generichash_blake2b_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_blake2b_statebytes

		function crypto_generichash_blake2b(out,outlen,in,inlen,key,keylen)&
		&bind(c,name='crypto_generichash_blake2b')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_size_t),value::outlen,keylen
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::key
		endfunction crypto_generichash_blake2b

		function crypto_generichash_blake2b_salt_personal(out,outlen,in,inlen,key,keylen,salt,personal)&
		&bind(c,name='crypto_generichash_blake2b_salt_personal')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_size_t),value::outlen,keylen
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::key,salt,personal
		endfunction crypto_generichash_blake2b_salt_personal

		function crypto_generichash_blake2b_init(state,key,keylen,outlen)&
		&bind(c,name='crypto_generichash_blake2b_init')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_size_t
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::key
			integer(kind=c_size_t),value::outlen,keylen
		endfunction crypto_generichash_blake2b_init

		function crypto_generichash_blake2b_init_salt_personal(state,key,keylen,outlen,salt,personal)&
		&bind(c,name='crypto_generichash_blake2b_init_salt_personal')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_size_t
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::key,salt,personal
			integer(kind=c_size_t),value::outlen,keylen
		endfunction crypto_generichash_blake2b_init_salt_personal

		function crypto_generichash_blake2b_update(state,in,inlen)&
		&bind(c,name='crypto_generichash_blake2b_update')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
		endfunction crypto_generichash_blake2b_update

		function crypto_generichash_blake2b_final(state,out,outlen)&
		&bind(c,name='crypto_generichash_blake2b_final')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_size_t
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::out
			integer(kind=c_size_t),value::outlen
		endfunction crypto_generichash_blake2b_final

		subroutine crypto_generichash_blake2b_keygen(k)&
		&bind(c,name='crypto_generichash_blake2b_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_generichash_blake2b_keygen

	endinterface

endmodule mod_crypto_generichash_blake2b