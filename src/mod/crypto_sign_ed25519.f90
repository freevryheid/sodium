module mod_crypto_sign_ed25519

	use,intrinsic::iso_c_binding
	use::mod_common
	use::mod_crypto_hash_sha512,only:crypto_hash_sha512_state

	implicit none

	private

	public::crypto_sign_ed25519ph_statebytes
	public::crypto_sign_ed25519_bytes
	public::crypto_sign_ed25519_seedbytes
	public::crypto_sign_ed25519_publickeybytes
	public::crypto_sign_ed25519_secretkeybytes
	public::crypto_sign_ed25519_messagebytes_max
	public::crypto_sign_ed25519
	public::crypto_sign_ed25519_open
	public::crypto_sign_ed25519_detached
	public::crypto_sign_ed25519_verify_detached
	public::crypto_sign_ed25519_keypair
	public::crypto_sign_ed25519_seed_keypair
	public::crypto_sign_ed25519_pk_to_curve25519
	public::crypto_sign_ed25519_sk_to_curve25519
	public::crypto_sign_ed25519_sk_to_seed
	public::crypto_sign_ed25519_sk_to_pk
	public::crypto_sign_ed25519ph_init
	public::crypto_sign_ed25519ph_update
	public::crypto_sign_ed25519ph_final_create
	public::crypto_sign_ed25519ph_final_verify

! #define crypto_sign_ed25519_BYTES 64U
! #define crypto_sign_ed25519_SEEDBYTES 32U
! #define crypto_sign_ed25519_PUBLICKEYBYTES 32U
! #define crypto_sign_ed25519_SECRETKEYBYTES (32U + 32U)
! #define crypto_sign_ed25519_MESSAGEBYTES_MAX (SODIUM_SIZE_MAX - crypto_sign_ed25519_BYTES)

	type,public,bind(c)::crypto_sign_ed25519ph_state
		type(crypto_hash_sha512_state)::hs
	endtype

	interface

		function crypto_sign_ed25519ph_statebytes()&
		&bind(c,name='crypto_sign_ed25519ph_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519ph_statebytes

		function crypto_sign_ed25519_bytes()&
		&bind(c,name='crypto_sign_ed25519_bytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519_bytes

		function crypto_sign_ed25519_seedbytes()&
		&bind(c,name='crypto_sign_ed25519_seedbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519_seedbytes

		function crypto_sign_ed25519_publickeybytes()&
		&bind(c,name='crypto_sign_ed25519_publickeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519_publickeybytes

		function crypto_sign_ed25519_secretkeybytes()&
		&bind(c,name='crypto_sign_ed25519_secretkeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519_secretkeybytes

		function crypto_sign_ed25519_messagebytes_max()&
		&bind(c,name='crypto_sign_ed25519_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_sign_ed25519_messagebytes_max

		function crypto_sign_ed25519(sm,smlen_p,m,mlen,sk)&
		&bind(c,name='crypto_sign_ed25519')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			integer(kind=c_long_long)::smlen_p
			character(kind=c_char)::sm
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::m,sk
		end function crypto_sign_ed25519

		function crypto_sign_ed25519_open(m,mlen_p,sm,smlen,pk)&
		&bind(c,name='crypto_sign_ed25519_open')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			integer(kind=c_long_long)::mlen_p
			character(kind=c_char)::m,sm
			integer(kind=c_long_long),value::smlen
			character(kind=c_char)::pk
		end function crypto_sign_ed25519_open

		function crypto_sign_ed25519_detached(sig,siglen_p,m,mlen,sk)&
		&bind(c,name='crypto_sign_ed25519_detached')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			integer(kind=c_long_long)::siglen_p
			character(kind=c_char)::sig,m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::sk
		end function crypto_sign_ed25519_detached

		function crypto_sign_ed25519_verify_detached(sig,m,mlen,pk)&
		&bind(c,name='crypto_sign_ed25519_verify_detached')&
		&result(res)
			import::c_int,c_long_long,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::sig,m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::pk
		end function crypto_sign_ed25519_verify_detached

		function crypto_sign_ed25519_keypair(pk,sk)&
		&bind(c,name='crypto_sign_ed25519_keypair')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
		end function crypto_sign_ed25519_keypair

		function crypto_sign_ed25519_seed_keypair(pk,sk,seed)&
		&bind(c,name='crypto_sign_ed25519_seed_keypair')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk,seed
		end function crypto_sign_ed25519_seed_keypair

		function crypto_sign_ed25519_pk_to_curve25519(curve25519_pk,ed25519_pk)&
		&bind(c,name='crypto_sign_ed25519_pk_to_curve25519')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::curve25519_pk,ed25519_pk
		end function crypto_sign_ed25519_pk_to_curve25519

		function crypto_sign_ed25519_sk_to_curve25519(curve25519_sk,ed25519_sk)&
		&bind(c,name='crypto_sign_ed25519_sk_to_curve25519')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::curve25519_sk,ed25519_sk
		end function crypto_sign_ed25519_sk_to_curve25519

		function crypto_sign_ed25519_sk_to_seed(seed,sk)&
		&bind(c,name='crypto_sign_ed25519_sk_to_seed')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::seed,sk
		end function crypto_sign_ed25519_sk_to_seed

		function crypto_sign_ed25519_sk_to_pk(pk,sk)&
		&bind(c,name='crypto_sign_ed25519_sk_to_pk')&
		&result(res)
			import::c_int,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
		end function crypto_sign_ed25519_sk_to_pk

		function crypto_sign_ed25519ph_init(state)&
		&bind(c,name='crypto_sign_ed25519ph_init')&
		&result(res)
			import::c_int,crypto_sign_ed25519ph_state
			integer(kind=c_int)::res
			type(crypto_sign_ed25519ph_state)::state
		end function crypto_sign_ed25519ph_init

		function crypto_sign_ed25519ph_update(state,m,mlen)&
		&bind(c,name='crypto_sign_ed25519ph_update')&
		&result(res)
			import::c_int,crypto_sign_ed25519ph_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_sign_ed25519ph_state)::state
			character(kind=c_char)::m
			integer(kind=c_long_long),value::mlen
		end function crypto_sign_ed25519ph_update

		function crypto_sign_ed25519ph_final_create(state,sig,siglen_p,sk)&
		&bind(c,name='crypto_sign_ed25519ph_final_create')&
		&result(res)
			import::c_int,crypto_sign_ed25519ph_state,c_char,c_long_long
			integer(kind=c_int)::res
			type(crypto_sign_ed25519ph_state)::state
			character(kind=c_char)::sig
			integer(kind=c_long_long)::siglen_p
			character(kind=c_char)::sk
		end function crypto_sign_ed25519ph_final_create

		function crypto_sign_ed25519ph_final_verify(state,sig,pk)&
		&bind(c,name='crypto_sign_ed25519ph_final_verify')&
		&result(res)
			import::c_int,crypto_sign_ed25519ph_state,c_char
			integer(kind=c_int)::res
			type(crypto_sign_ed25519ph_state)::state
			character(kind=c_char)::sig
			character(kind=c_char)::pk
		end function crypto_sign_ed25519ph_final_verify

	endinterface

endmodule mod_crypto_sign_ed25519
