module mod_crypto_sign_edwards25519sha512batch

	use,intrinsic::iso_c_binding
	! use::mod_common
	! use::mod_crypto_hash_sha512,only:crypto_hash_sha512_state

	implicit none

	private

	public::crypto_sign_edwards25519sha512batch
	public::crypto_sign_edwards25519sha512batch_open
	public::crypto_sign_edwards25519sha512batch_keypair

	! #define crypto_sign_edwards25519sha512batch_BYTES 64U
	! #define crypto_sign_edwards25519sha512batch_PUBLICKEYBYTES 32U
	! #define crypto_sign_edwards25519sha512batch_SECRETKEYBYTES (32U + 32U)
	! #define crypto_sign_edwards25519sha512batch_MESSAGEBYTES_MAX (SODIUM_SIZE_MAX - crypto_sign_edwards25519sha512batch_BYTES)

	interface

		function crypto_sign_edwards25519sha512batch(sm,smlen_p,m,mlen,sk)&
		&bind(c,name='crypto_sign_edwards25519sha512batch')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			integer(kind=c_long_long)::smlen_p
			character(kind=c_char)::sm
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::m,sk
		endfunction crypto_sign_edwards25519sha512batch

		function crypto_sign_edwards25519sha512batch_open(m,mlen_p,sm,smlen,pk)&
		&bind(c,name='crypto_sign_edwards25519sha512batch_open')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			integer(kind=c_long_long)::mlen_p
			character(kind=c_char)::m,sm
			integer(kind=c_long_long),value::smlen
			character(kind=c_char)::pk
		endfunction crypto_sign_edwards25519sha512batch_open

		function crypto_sign_edwards25519sha512batch_keypair(pk,sk)&
		&bind(c,name='crypto_sign_edwards25519sha512batch_keypair')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
		endfunction crypto_sign_edwards25519sha512batch_keypair

	endinterface

endmodule mod_crypto_sign_edwards25519sha512batch
