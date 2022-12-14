module mod_crypto_generichash

	use,intrinsic::iso_c_binding
	use::mod_crypto_generichash_blake2b
	use::mod_common

	implicit none

	private

	public::crypto_generichash_bytes_min
	public::crypto_generichash_bytes_max
	public::crypto_generichash_bytes
	public::crypto_generichash_keybytes_min
	public::crypto_generichash_keybytes_max
	public::crypto_generichash_keybytes
	public::crypto_generichash_primitive
	public::crypto_generichash_statebytes
	public::crypto_generichash
	public::crypto_generichash_init
	public::crypto_generichash_update
	public::crypto_generichash_final
	public::crypto_generichash_keygen

	! #define crypto_generichash_BYTES_MIN crypto_generichash_blake2b_BYTES_MIN
	! #define crypto_generichash_BYTES_MAX crypto_generichash_blake2b_BYTES_MAX
	! #define crypto_generichash_KEYBYTES_MAX crypto_generichash_blake2b_KEYBYTES_MAX
	! #define crypto_generichash_KEYBYTES crypto_generichash_blake2b_KEYBYTES
	! #define crypto_generichash_PRIMITIVE "blake2b"
	! #define crypto_generichash_BYTES crypto_generichash_blake2b_BYTES
	! #define crypto_generichash_KEYBYTES_MIN crypto_generichash_blake2b_KEYBYTES_MIN

	interface

		function crypto_generichash_bytes_min()&
		&bind(c,name='crypto_generichash_bytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_bytes_min

		function crypto_generichash_bytes_max()&
		&bind(c,name='crypto_generichash_bytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_bytes_max

		function crypto_generichash_bytes()&
		&bind(c,name='crypto_generichash_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_bytes

		function crypto_generichash_keybytes_min()&
		&bind(c,name='crypto_generichash_keybytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_keybytes_min

		function crypto_generichash_keybytes_max()&
		&bind(c,name='crypto_generichash_keybytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_keybytes_max

		function crypto_generichash_keybytes()&
		&bind(c,name='crypto_generichash_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_keybytes

		function bind_crypto_generichash_primitive()&
		&bind(c,name='crypto_generichash_primitive')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_generichash_primitive

		function crypto_generichash_statebytes()&
		&bind(c,name='crypto_generichash_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_generichash_statebytes

		function crypto_generichash(out,outlen,in,inlen,key,keylen)&
		&bind(c,name='crypto_generichash')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_size_t),value::outlen,keylen
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::key
		endfunction crypto_generichash

		function crypto_generichash_init(state,key,keylen,outlen)&
		&bind(c,name='crypto_generichash_init')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_size_t
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::key
			integer(kind=c_size_t),value::outlen,keylen
		endfunction crypto_generichash_init

		function crypto_generichash_update(state,in,inlen)&
		&bind(c,name='crypto_generichash_update')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
		endfunction crypto_generichash_update

		function crypto_generichash_final(state,out,outlen)&
		&bind(c,name='crypto_generichash_final')&
		&result(res)
			import::c_int,crypto_generichash_blake2b_state,c_char,c_size_t
			integer(kind=c_int)::res
			type(crypto_generichash_blake2b_state)::state
			character(kind=c_char)::out
			integer(kind=c_size_t),value::outlen
		endfunction crypto_generichash_final

		subroutine crypto_generichash_keygen(k)&
		&bind(c,name='crypto_generichash_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_generichash_keygen

	endinterface

	contains

		function crypto_generichash_primitive()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_generichash_primitive()
			call c_f_str_ptr(res1,res)
		endfunction crypto_generichash_primitive

endmodule mod_crypto_generichash