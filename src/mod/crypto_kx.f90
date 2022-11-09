module mod_crypto_kx

	use,intrinsic::iso_c_binding
	use::mod_common

	implicit none

	private

	public::crypto_kx_publickeybytes
	public::crypto_kx_secretkeybytes
	public::crypto_kx_seedbytes
	public::crypto_kx_sessionkeybytes
	public::crypto_kx_primitive
	public::crypto_kx_seed_keypair
	public::crypto_kx_keypair
	public::crypto_kx_client_session_keys
	public::crypto_kx_server_session_keys

	! #define crypto_kx_PUBLICKEYBYTES 32
	! #define crypto_kx_SECRETKEYBYTES 32
	! #define crypto_kx_SEEDBYTES 32
	! #define crypto_kx_SESSIONKEYBYTES 32
	! #define crypto_kx_PRIMITIVE "x25519blake2b"

	interface

		function crypto_kx_publickeybytes()&
		&bind(c,name='crypto_kx_publickeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_kx_publickeybytes

		function crypto_kx_secretkeybytes()&
		&bind(c,name='crypto_kx_secretkeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_kx_secretkeybytes

		function crypto_kx_seedbytes()&
		&bind(c,name='crypto_kx_seedbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_kx_seedbytes

		function crypto_kx_sessionkeybytes()&
		&bind(c,name='crypto_kx_sessionkeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_kx_sessionkeybytes

		function bind_crypto_kx_primitive()&
		&bind(c,name='crypto_kx_primitive')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_kx_primitive

		function crypto_kx_seed_keypair(pk,sk,seed)&
		&bind(c,name='crypto_kx_seed_keypair')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
			character(kind=c_char)::seed
		endfunction crypto_kx_seed_keypair

		function crypto_kx_keypair(pk,sk)&
		&bind(c,name='crypto_kx_keypair')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
		endfunction crypto_kx_keypair

		! TODO: testme
		function crypto_kx_client_session_keys(rx,tx,client_pk,client_sk,server_pk)&
		&bind(c,name='crypto_kx_client_session_keys')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::rx,tx
			character(kind=c_char)::client_pk,client_sk,server_pk
		endfunction crypto_kx_client_session_keys

		! TODO: testme
		function crypto_kx_server_session_keys(rx,tx,server_pk,server_sk,client_pk)&
		&bind(c,name='crypto_kx_server_session_keys')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::rx,tx
			character(kind=c_char)::client_pk,server_pk,server_sk
		endfunction crypto_kx_server_session_keys

	endinterface

	contains

		function crypto_kx_primitive()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_kx_primitive()
			call c_f_str_ptr(res1,res)
		endfunction crypto_kx_primitive

endmodule mod_crypto_kx