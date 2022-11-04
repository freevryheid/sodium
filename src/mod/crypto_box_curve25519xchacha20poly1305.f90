module mod_crypto_box_curve25519xchacha20poly1305

	use,intrinsic::iso_c_binding
	! use::mod_common

	implicit none

	private

	public::crypto_box_curve25519xchacha20poly1305_seedbytes
	public::crypto_box_curve25519xchacha20poly1305_publickeybytes
	public::crypto_box_curve25519xchacha20poly1305_secretkeybytes
	public::crypto_box_curve25519xchacha20poly1305_beforenmbytes
	public::crypto_box_curve25519xchacha20poly1305_noncebytes
	public::crypto_box_curve25519xchacha20poly1305_macbytes
	public::crypto_box_curve25519xchacha20poly1305_messagebytes_max
	public::crypto_box_curve25519xchacha20poly1305_seed_keypair
	public::crypto_box_curve25519xchacha20poly1305_keypair
	public::crypto_box_curve25519xchacha20poly1305_easy
	public::crypto_box_curve25519xchacha20poly1305_open_easy
	public::crypto_box_curve25519xchacha20poly1305_detached
	public::crypto_box_curve25519xchacha20poly1305_open_detached
	public::crypto_box_curve25519xchacha20poly1305_beforenm
	public::crypto_box_curve25519xchacha20poly1305_easy_afternm
	public::crypto_box_curve25519xchacha20poly1305_open_easy_afternm
	public::crypto_box_curve25519xchacha20poly1305_detached_afternm
	public::crypto_box_curve25519xchacha20poly1305_open_detached_afternm
	public::crypto_box_curve25519xchacha20poly1305_sealbytes
	public::crypto_box_curve25519xchacha20poly1305_seal
	public::crypto_box_curve25519xchacha20poly1305_seal_open

 ! #define crypto_box_curve25519xchacha20poly1305_SEEDBYTES 32U
 ! #define crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES 32U
 ! #define crypto_box_curve25519xchacha20poly1305_SECRETKEYBYTES 32U
 ! #define crypto_box_curve25519xchacha20poly1305_BEFORENMBYTES 32U
 ! #define crypto_box_curve25519xchacha20poly1305_NONCEBYTES 24U
 ! #define crypto_box_curve25519xchacha20poly1305_MACBYTES 16U


	interface

		function crypto_box_curve25519xchacha20poly1305_seedbytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_seedbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_seedbytes

		function crypto_box_curve25519xchacha20poly1305_publickeybytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_publickeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_publickeybytes

		function crypto_box_curve25519xchacha20poly1305_secretkeybytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_secretkeybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_secretkeybytes

		function crypto_box_curve25519xchacha20poly1305_beforenmbytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_beforenmbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_beforenmbytes

		function crypto_box_curve25519xchacha20poly1305_noncebytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_noncebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_noncebytes

		function crypto_box_curve25519xchacha20poly1305_macbytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_macbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_macbytes

		function crypto_box_curve25519xchacha20poly1305_messagebytes_max()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_messagebytes_max

		function crypto_box_curve25519xchacha20poly1305_seed_keypair(pk,sk,seed)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_seed_keypair')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
			character(kind=c_char)::seed
		endfunction crypto_box_curve25519xchacha20poly1305_seed_keypair

		function crypto_box_curve25519xchacha20poly1305_keypair(pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_keypair')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_keypair

		function crypto_box_curve25519xchacha20poly1305_easy(c,m,mlen,n,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_easy')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_easy

		function crypto_box_curve25519xchacha20poly1305_open_easy(m,c,clen,n,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_open_easy')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_open_easy

		function crypto_box_curve25519xchacha20poly1305_detached(c,mac,m,mlen,n,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_detached')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::mac,m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_detached

		function crypto_box_curve25519xchacha20poly1305_open_detached(m,c,mac,clen,n,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_open_detached')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c,mac
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_open_detached

		function crypto_box_curve25519xchacha20poly1305_beforenm(k,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_beforenm')&
		&result(res)
			import::c_char,c_int
			integer(kind=c_int)::res
			character(kind=c_char)::k
			character(kind=c_char)::pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_beforenm

		function crypto_box_curve25519xchacha20poly1305_easy_afternm(c,m,mlen,n,k)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_easy_afternm')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_box_curve25519xchacha20poly1305_easy_afternm

		function crypto_box_curve25519xchacha20poly1305_open_easy_afternm(m,c,clen,n,k)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_open_easy_afternm')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_box_curve25519xchacha20poly1305_open_easy_afternm

		function crypto_box_curve25519xchacha20poly1305_detached_afternm(c,mac,m,mlen,n,k)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_detached_afternm')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::mac,m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::n,k
		endfunction crypto_box_curve25519xchacha20poly1305_detached_afternm

		function crypto_box_curve25519xchacha20poly1305_open_detached_afternm(m,c,mac,clen,n,k)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_open_detached_afternm')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c,mac
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::n,k
		endfunction crypto_box_curve25519xchacha20poly1305_open_detached_afternm

		function crypto_box_curve25519xchacha20poly1305_sealbytes()&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_sealbytes')&
		&result(res)
			import c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_box_curve25519xchacha20poly1305_sealbytes

		function crypto_box_curve25519xchacha20poly1305_seal(c,m,mlen,pk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_seal')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::c
			character(kind=c_char)::m
			integer(kind=c_long_long),value::mlen
			character(kind=c_char)::pk
		endfunction crypto_box_curve25519xchacha20poly1305_seal

		function crypto_box_curve25519xchacha20poly1305_seal_open(m,c,clen,pk,sk)&
		&bind(c,name='crypto_box_curve25519xchacha20poly1305_seal_open')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::m
			character(kind=c_char)::c
			integer(kind=c_long_long),value::clen
			character(kind=c_char)::pk,sk
		endfunction crypto_box_curve25519xchacha20poly1305_seal_open

	endinterface

endmodule mod_crypto_box_curve25519xchacha20poly1305
