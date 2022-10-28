module mod_crypto_secretstream_xchacha20poly1305

	use,intrinsic::iso_c_binding

	implicit none

	private

	integer,public,parameter::CRYPTO_STREAM_CHACHA20_IETF_KEYBYTES=32
	integer,public,parameter::CRYPTO_STREAM_CHACHA20_IETF_NONCEBYTES=12

	! these are not really needed as we have functions that define these parameters
	! integer(kind=c_signed_char),public,parameter::CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_MESSAGE_=0
	! integer(kind=c_signed_char),public,parameter::CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_PUSH_=1
	! integer(kind=c_signed_char),public,parameter::CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_REKEY_=2
	! integer(kind=c_signed_char),public,parameter::CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_FINAL_=ior(&
	! &CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_PUSH_,CRYPTO_SECRETSTREAM_XCHACHA20POLY1305_TAG_REKEY_)

	! thank you modern fortran
	type,public,bind(c)::crypto_secretstream_xchacha20poly1305_state
		character(kind=c_char)::k(CRYPTO_STREAM_CHACHA20_IETF_KEYBYTES)
		character(kind=c_char)::nonce(CRYPTO_STREAM_CHACHA20_IETF_NONCEBYTES)
		character(kind=c_char)::pad(8)
	endtype

	public::crypto_secretstream_xchacha20poly1305_abytes
	public::crypto_secretstream_xchacha20poly1305_headerbytes
	public::crypto_secretstream_xchacha20poly1305_keybytes
	public::crypto_secretstream_xchacha20poly1305_messagebytes_max
	public::crypto_secretstream_xchacha20poly1305_tag_message
	public::crypto_secretstream_xchacha20poly1305_tag_push
	public::crypto_secretstream_xchacha20poly1305_tag_rekey
	public::crypto_secretstream_xchacha20poly1305_tag_final
	public::crypto_secretstream_xchacha20poly1305_statebytes
	public::crypto_secretstream_xchacha20poly1305_keygen
	public::crypto_secretstream_xchacha20poly1305_init_push
	public::crypto_secretstream_xchacha20poly1305_push
	public::crypto_secretstream_xchacha20poly1305_init_pull
	public::crypto_secretstream_xchacha20poly1305_pull
	public::crypto_secretstream_xchacha20poly1305_rekey

	interface

		function crypto_secretstream_xchacha20poly1305_abytes()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_abytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretstream_xchacha20poly1305_abytes

		function crypto_secretstream_xchacha20poly1305_headerbytes()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_headerbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretstream_xchacha20poly1305_headerbytes

		function crypto_secretstream_xchacha20poly1305_keybytes()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_keybytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretstream_xchacha20poly1305_keybytes

		function crypto_secretstream_xchacha20poly1305_messagebytes_max()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_messagebytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretstream_xchacha20poly1305_messagebytes_max

		function crypto_secretstream_xchacha20poly1305_tag_message()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_tag_message')&
		&result(res)
			import::c_signed_char
			integer(kind=c_signed_char)::res
		endfunction crypto_secretstream_xchacha20poly1305_tag_message

		function crypto_secretstream_xchacha20poly1305_tag_push()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_tag_push')&
		&result(res)
			import::c_signed_char
			integer(kind=c_signed_char)::res
		endfunction crypto_secretstream_xchacha20poly1305_tag_push

		function crypto_secretstream_xchacha20poly1305_tag_rekey()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_tag_rekey')&
		&result(res)
			import::c_signed_char
			integer(kind=c_signed_char)::res
		endfunction crypto_secretstream_xchacha20poly1305_tag_rekey

		function crypto_secretstream_xchacha20poly1305_tag_final()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_tag_final')&
		&result(res)
			import::c_signed_char
			integer(kind=c_signed_char)::res
		endfunction crypto_secretstream_xchacha20poly1305_tag_final

		function crypto_secretstream_xchacha20poly1305_statebytes()&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_statebytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_secretstream_xchacha20poly1305_statebytes

		subroutine crypto_secretstream_xchacha20poly1305_keygen(k)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_keygen')
			import::c_char
			character(kind=c_char)::k
		endsubroutine crypto_secretstream_xchacha20poly1305_keygen

		function crypto_secretstream_xchacha20poly1305_init_push(state,header,k)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_init_push')&
		&result(res)
			import::c_int,c_char,crypto_secretstream_xchacha20poly1305_state
			integer(kind=c_int)::res
			type(crypto_secretstream_xchacha20poly1305_state)::state
			character(kind=c_char)::header
			character(kind=c_char)::k
		endfunction crypto_secretstream_xchacha20poly1305_init_push

		function crypto_secretstream_xchacha20poly1305_push(state,c,clen,m,mlen,ad,adlen,tag)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_push')&
		&result(res)
			import::c_int,c_char,c_signed_char,c_long_long,crypto_secretstream_xchacha20poly1305_state
			integer(kind=c_int)::res
			type(crypto_secretstream_xchacha20poly1305_state)::state
			integer(kind=c_long_long)::clen
			character(kind=c_char)::c
			integer(kind=c_long_long),value::mlen,adlen
			character(kind=c_char)::m,ad
			integer(kind=c_signed_char),value::tag
		endfunction crypto_secretstream_xchacha20poly1305_push

		function crypto_secretstream_xchacha20poly1305_init_pull(state,header,k)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_init_pull')&
		&result(res)
			import::c_int,c_char,crypto_secretstream_xchacha20poly1305_state
			integer(kind=c_int)::res
			type(crypto_secretstream_xchacha20poly1305_state)::state
			character(kind=c_char)::header
			character(kind=c_char)::k
		endfunction crypto_secretstream_xchacha20poly1305_init_pull

		function crypto_secretstream_xchacha20poly1305_pull(state,m,mlen,tag,c,clen,ad,adlen)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_pull')&
		&result(res)
			import::c_int,c_char,c_signed_char,c_long_long,crypto_secretstream_xchacha20poly1305_state
			integer(kind=c_int)::res
			type(crypto_secretstream_xchacha20poly1305_state)::state
			integer(kind=c_long_long)::mlen
			character(kind=c_char)::m
			integer(kind=c_long_long),value::clen,adlen
			character(kind=c_char)::c,ad
			integer(kind=c_signed_char)::tag
		endfunction crypto_secretstream_xchacha20poly1305_pull

		subroutine crypto_secretstream_xchacha20poly1305_rekey(state)&
		&bind(c,name='crypto_secretstream_xchacha20poly1305_rekey')
			import::crypto_secretstream_xchacha20poly1305_state
			type(crypto_secretstream_xchacha20poly1305_state)::state
		endsubroutine crypto_secretstream_xchacha20poly1305_rekey

	endinterface

endmodule mod_crypto_secretstream_xchacha20poly1305
