module mod_crypto_shorthash

	use,intrinsic::iso_c_binding
	! use::mod_crypto_generichash_blake2b
	use::mod_common

	implicit none

	private

	public::crypto_shorthash_bytes
	public::crypto_shorthash_keybytes
	public::crypto_shorthash_primitive
	public::crypto_shorthash
	public::crypto_shorthash_keygen

 ! #define crypto_shorthash_BYTES crypto_shorthash_siphash24_BYTES
 ! #define crypto_shorthash_KEYBYTES crypto_shorthash_siphash24_KEYBYTES
 ! #define crypto_shorthash_PRIMITIVE "siphash24"

	interface

		function crypto_shorthash_bytes()&
		&bind(c,name='crypto_shorthash_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_shorthash_bytes

		function crypto_shorthash_keybytes()&
		&bind(c,name='crypto_shorthash_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_shorthash_keybytes

		function bind_crypto_shorthash_primitive()&
		&bind(c,name='crypto_shorthash_primitive')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_shorthash_primitive

		function crypto_shorthash(out,in,inlen,k)&
		&bind(c,name='crypto_shorthash')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
			character(kind=c_char)::k
		endfunction crypto_shorthash

		subroutine crypto_shorthash_keygen(k)&
		&bind(c,name='crypto_shorthash_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_shorthash_keygen

	 endinterface

	 contains

		function crypto_shorthash_primitive()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_shorthash_primitive()
			call c_f_str_ptr(res1,res)
		endfunction crypto_shorthash_primitive

endmodule mod_crypto_shorthash
