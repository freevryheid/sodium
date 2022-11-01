module mod_crypto_hash_sha256

	use,intrinsic::iso_c_binding
	! use::mod_common

	implicit none

	private

	public::crypto_hash_sha256_statebytes
	public::crypto_hash_sha256_bytes
	public::crypto_hash_sha256
	public::crypto_hash_sha256_init
	public::crypto_hash_sha256_update
	public::crypto_hash_sha256_final

! #define crypto_hash_sha256_BYTES 32U

	! TODO: perhaps bump these up to match unsigned equivalents
	type,public,bind(c)::crypto_hash_sha256_state
		integer(kind=c_int32_t)::state(8)
		integer(kind=c_int64_t)::count
		integer(kind=c_int8_t)::buf(64)
	endtype

	interface

		function crypto_hash_sha256_statebytes()&
		&bind(c,name='crypto_hash_sha256_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_hash_sha256_statebytes

		function crypto_hash_sha256_bytes()&
		&bind(c,name='crypto_hash_sha256_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_hash_sha256_bytes

		function crypto_hash_sha256(out,in,inlen)&
		&bind(c,name='crypto_hash_sha256')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
		end function crypto_hash_sha256

		function crypto_hash_sha256_init(state)&
		&bind(c,name='crypto_hash_sha256_init')&
		&result(res)
			import::c_int,crypto_hash_sha256_state
			integer(kind=c_int)::res
			type(crypto_hash_sha256_state)::state
		end function crypto_hash_sha256_init

		function crypto_hash_sha256_update(state,in,inlen)&
		&bind(c,name='crypto_hash_sha256_update')&
		&result(res)
			import::c_int,crypto_hash_sha256_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_hash_sha256_state)::state
			character(kind=c_char)::in
			integer(kind=c_long_long),value::inlen
		end function crypto_hash_sha256_update

		function crypto_hash_sha256_final(state,out)&
		&bind(c,name='crypto_hash_sha256_final')&
		&result(res)
			import::c_int,crypto_hash_sha256_state,c_char
			integer(kind=c_int)::res
			type(crypto_hash_sha256_state)::state
			character(kind=c_char)::out
		end function crypto_hash_sha256_final

	endinterface

endmodule mod_crypto_hash_sha256
