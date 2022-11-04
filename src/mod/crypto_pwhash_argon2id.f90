module mod_crypto_pwhash_argon2id

	use,intrinsic::iso_c_binding
	! use::mod_crypto_generichash_blake2b
	use::mod_common

	implicit none

	private

	public::crypto_pwhash_argon2id_alg_argon2id13
	public::crypto_pwhash_argon2id_bytes_min
	public::crypto_pwhash_argon2id_bytes_max
	public::crypto_pwhash_argon2id_passwd_min
	public::crypto_pwhash_argon2id_passwd_max
	public::crypto_pwhash_argon2id_saltbytes
	public::crypto_pwhash_argon2id_strbytes
	public::crypto_pwhash_argon2id_strprefix
	public::crypto_pwhash_argon2id_opslimit_min
	public::crypto_pwhash_argon2id_opslimit_max
	public::crypto_pwhash_argon2id_memlimit_min
	public::crypto_pwhash_argon2id_memlimit_max
	public::crypto_pwhash_argon2id_opslimit_interactive
	public::crypto_pwhash_argon2id_memlimit_interactive
	public::crypto_pwhash_argon2id_opslimit_moderate
	public::crypto_pwhash_argon2id_memlimit_moderate
	public::crypto_pwhash_argon2id_opslimit_sensitive
	public::crypto_pwhash_argon2id_memlimit_sensitive
	public::crypto_pwhash_argon2id
	public::crypto_pwhash_argon2id_str
	public::crypto_pwhash_argon2id_str_verify
	public::crypto_pwhash_argon2id_str_needs_rehash

	! #define crypto_pwhash_argon2id_ALG_ARGON2ID13 2
	! #define crypto_pwhash_argon2id_BYTES_MIN 16U
	! #define crypto_pwhash_argon2id_BYTES_MAX SODIUM_MIN(SODIUM_SIZE_MAX,4294967295U)
	! #define crypto_pwhash_argon2id_PASSWD_MIN 0U
	! #define crypto_pwhash_argon2id_PASSWD_MAX 4294967295U
	! #define crypto_pwhash_argon2id_SALTBYTES 16U
	! #define crypto_pwhash_argon2id_STRBYTES 128U
	! #define crypto_pwhash_argon2id_STRPREFIX "$argon2id$"
	! #define crypto_pwhash_argon2id_OPSLIMIT_MIN 1U
	! #define crypto_pwhash_argon2id_OPSLIMIT_MAX 4294967295U
	! #define crypto_pwhash_argon2id_MEMLIMIT_MIN 8192U
	! #define crypto_pwhash_argon2id_OPSLIMIT_INTERACTIVE 2U
	! #define crypto_pwhash_argon2id_MEMLIMIT_INTERACTIVE 67108864U
	! #define crypto_pwhash_argon2id_OPSLIMIT_MODERATE 3U
	! #define crypto_pwhash_argon2id_MEMLIMIT_MODERATE 268435456U
	! #define crypto_pwhash_argon2id_OPSLIMIT_SENSITIVE 4U
	! #define crypto_pwhash_argon2id_MEMLIMIT_SENSITIVE 1073741824U

	interface

		function crypto_pwhash_argon2id_alg_argon2id13()&
		&bind(c,name='crypto_pwhash_argon2id_alg_argon2id13')&
		&result(res)
			import::c_int
			integer(kind=c_int)::res
		endfunction crypto_pwhash_argon2id_alg_argon2id13

		function crypto_pwhash_argon2id_bytes_min()&
		&bind(c,name='crypto_pwhash_argon2id_bytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_bytes_min

		function crypto_pwhash_argon2id_bytes_max()&
		&bind(c,name='crypto_pwhash_argon2id_bytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_bytes_max

		function crypto_pwhash_argon2id_passwd_min()&
		&bind(c,name='crypto_pwhash_argon2id_passwd_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_passwd_min

		function crypto_pwhash_argon2id_passwd_max()&
		&bind(c,name='crypto_pwhash_argon2id_passwd_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_passwd_max

		function crypto_pwhash_argon2id_saltbytes()&
		&bind(c,name='crypto_pwhash_argon2id_saltbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_saltbytes

		function crypto_pwhash_argon2id_strbytes()&
		&bind(c,name='crypto_pwhash_argon2id_strbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_strbytes

		function bind_crypto_pwhash_argon2id_strprefix()&
		&bind(c,name='crypto_pwhash_argon2id_strprefix')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_pwhash_argon2id_strprefix

		function crypto_pwhash_argon2id_opslimit_min()&
		&bind(c,name='crypto_pwhash_argon2id_opslimit_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_opslimit_min

		function crypto_pwhash_argon2id_opslimit_max()&
		&bind(c,name='crypto_pwhash_argon2id_opslimit_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_opslimit_max

		function crypto_pwhash_argon2id_memlimit_min()&
		&bind(c,name='crypto_pwhash_argon2id_memlimit_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_memlimit_min

		function crypto_pwhash_argon2id_memlimit_max()&
		&bind(c,name='crypto_pwhash_argon2id_memlimit_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_memlimit_max

		function crypto_pwhash_argon2id_opslimit_interactive()&
		&bind(c,name='crypto_pwhash_argon2id_opslimit_interactive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_opslimit_interactive

		function crypto_pwhash_argon2id_memlimit_interactive()&
		&bind(c,name='crypto_pwhash_argon2id_memlimit_interactive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_memlimit_interactive

		function crypto_pwhash_argon2id_opslimit_moderate()&
		&bind(c,name='crypto_pwhash_argon2id_opslimit_moderate')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_opslimit_moderate

		function crypto_pwhash_argon2id_memlimit_moderate()&
		&bind(c,name='crypto_pwhash_argon2id_memlimit_moderate')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_memlimit_moderate

		function crypto_pwhash_argon2id_opslimit_sensitive()&
		&bind(c,name='crypto_pwhash_argon2id_opslimit_sensitive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_opslimit_sensitive

		function crypto_pwhash_argon2id_memlimit_sensitive()&
		&bind(c,name='crypto_pwhash_argon2id_memlimit_sensitive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_argon2id_memlimit_sensitive

		function crypto_pwhash_argon2id(out,outlen,passwd,passwdlen,salt,opslimit,memlimit,alg)&
		&bind(c,name='crypto_pwhash_argon2id')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::passwd
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::outlen,passwdlen,opslimit
			character(kind=c_char)::salt
			integer(kind=c_int),value::alg
		endfunction crypto_pwhash_argon2id

		function crypto_pwhash_argon2id_str(out,passwd,passwdlen,opslimit,memlimit)&
		&bind(c,name='crypto_pwhash_argon2id_str')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::passwd
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::passwdlen,opslimit
		endfunction crypto_pwhash_argon2id_str

		function crypto_pwhash_argon2id_str_verify(str,passwd,passwdlen)&
		&bind(c,name='crypto_pwhash_argon2id_str_verify')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::str
			character(kind=c_char)::passwd
			integer(kind=c_long_long),value::passwdlen
		endfunction crypto_pwhash_argon2id_str_verify

		function crypto_pwhash_argon2id_str_needs_rehash(str,opslimit,memlimit)&
		&bind(c,name='crypto_pwhash_argon2id_str_needs_rehash')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::str
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::opslimit
		endfunction crypto_pwhash_argon2id_str_needs_rehash

	endinterface

	contains

		function crypto_pwhash_argon2id_strprefix()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_pwhash_argon2id_strprefix()
			call c_f_str_ptr(res1,res)
		endfunction crypto_pwhash_argon2id_strprefix

endmodule mod_crypto_pwhash_argon2id
