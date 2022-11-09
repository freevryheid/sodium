module mod_crypto_pwhash_scryptsalsa208sha256

	use,intrinsic::iso_c_binding
	use::mod_common

	implicit none

	private

	public::crypto_pwhash_scryptsalsa208sha256_bytes_min
	public::crypto_pwhash_scryptsalsa208sha256_bytes_max
	public::crypto_pwhash_scryptsalsa208sha256_passwd_min
	public::crypto_pwhash_scryptsalsa208sha256_passwd_max
	public::crypto_pwhash_scryptsalsa208sha256_saltbytes
	public::crypto_pwhash_scryptsalsa208sha256_strbytes
	public::crypto_pwhash_scryptsalsa208sha256_strprefix
	public::crypto_pwhash_scryptsalsa208sha256_opslimit_min
	public::crypto_pwhash_scryptsalsa208sha256_opslimit_max
	public::crypto_pwhash_scryptsalsa208sha256_memlimit_min
	public::crypto_pwhash_scryptsalsa208sha256_memlimit_max
	public::crypto_pwhash_scryptsalsa208sha256_opslimit_interactive
	public::crypto_pwhash_scryptsalsa208sha256_memlimit_interactive
	public::crypto_pwhash_scryptsalsa208sha256_opslimit_sensitive
	public::crypto_pwhash_scryptsalsa208sha256_memlimit_sensitive
	public::crypto_pwhash_scryptsalsa208sha256
	public::crypto_pwhash_scryptsalsa208sha256_str
	public::crypto_pwhash_scryptsalsa208sha256_str_verify
	public::crypto_pwhash_scryptsalsa208sha256_ll
	public::crypto_pwhash_scryptsalsa208sha256_str_needs_rehash

	! #define crypto_pwhash_scryptsalsa208sha256_BYTES_MIN 16U
	! #define crypto_pwhash_scryptsalsa208sha256_PASSWD_MIN 0U
	! #define crypto_pwhash_scryptsalsa208sha256_PASSWD_MAX SODIUM_SIZE_MAX
	! #define crypto_pwhash_scryptsalsa208sha256_SALTBYTES 32U
	! #define crypto_pwhash_scryptsalsa208sha256_STRBYTES 102U
	! #define crypto_pwhash_scryptsalsa208sha256_STRPREFIX "$7$"
	! #define crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_MIN 32768U
	! #define crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_MAX 4294967295U
	! #define crypto_pwhash_scryptsalsa208sha256_MEMLIMIT_MIN 16777216U
	! #define crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_INTERACTIVE 524288U
	! #define crypto_pwhash_scryptsalsa208sha256_MEMLIMIT_INTERACTIVE 16777216U
	! #define crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_SENSITIVE 33554432U
	! #define crypto_pwhash_scryptsalsa208sha256_MEMLIMIT_SENSITIVE 1073741824U

	interface

		function crypto_pwhash_scryptsalsa208sha256_bytes_min()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_bytes_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_bytes_min

		function crypto_pwhash_scryptsalsa208sha256_bytes_max()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_bytes_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_bytes_max

		function crypto_pwhash_scryptsalsa208sha256_passwd_min()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_passwd_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_passwd_min

		function crypto_pwhash_scryptsalsa208sha256_passwd_max()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_passwd_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_passwd_max

		function crypto_pwhash_scryptsalsa208sha256_saltbytes()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_saltbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_saltbytes

		function crypto_pwhash_scryptsalsa208sha256_strbytes()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_strbytes')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_strbytes

		function bind_crypto_pwhash_scryptsalsa208sha256_strprefix()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_strprefix')&
		&result(res)
			import::c_ptr
			type(c_ptr)::res
		endfunction bind_crypto_pwhash_scryptsalsa208sha256_strprefix

		function crypto_pwhash_scryptsalsa208sha256_opslimit_min()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_opslimit_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_opslimit_min

		function crypto_pwhash_scryptsalsa208sha256_opslimit_max()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_opslimit_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_opslimit_max

		function crypto_pwhash_scryptsalsa208sha256_memlimit_min()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_memlimit_min')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_memlimit_min

		function crypto_pwhash_scryptsalsa208sha256_memlimit_max()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_memlimit_max')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_memlimit_max

		function crypto_pwhash_scryptsalsa208sha256_opslimit_interactive()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_opslimit_interactive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_opslimit_interactive

		function crypto_pwhash_scryptsalsa208sha256_memlimit_interactive()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_memlimit_interactive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_memlimit_interactive

		function crypto_pwhash_scryptsalsa208sha256_opslimit_sensitive()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_opslimit_sensitive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_opslimit_sensitive

		function crypto_pwhash_scryptsalsa208sha256_memlimit_sensitive()&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_memlimit_sensitive')&
		&result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		endfunction crypto_pwhash_scryptsalsa208sha256_memlimit_sensitive

		function crypto_pwhash_scryptsalsa208sha256(out,outlen,passwd,passwdlen,salt,opslimit,memlimit)&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::passwd
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::outlen,passwdlen,opslimit
			character(kind=c_char)::salt
		endfunction crypto_pwhash_scryptsalsa208sha256

		function crypto_pwhash_scryptsalsa208sha256_str(out,passwd,passwdlen,opslimit,memlimit)&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_str')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::out
			character(kind=c_char)::passwd
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::passwdlen,opslimit
		endfunction crypto_pwhash_scryptsalsa208sha256_str

		function crypto_pwhash_scryptsalsa208sha256_str_verify(str,passwd,passwdlen)&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_str_verify')&
		&result(res)
			import::c_char,c_int,c_long_long
			integer(kind=c_int)::res
			character(kind=c_char)::str
			character(kind=c_char)::passwd
			integer(kind=c_long_long),value::passwdlen
		endfunction crypto_pwhash_scryptsalsa208sha256_str_verify

		! TODO- testme
		function crypto_pwhash_scryptsalsa208sha256_ll(passwd,passwdlen,salt,saltlen,n,r,p,buf,buflen)&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_ll')&
		&result(res)
			import::c_int,c_int8_t,c_size_t,c_int32_t,c_int64_t
			integer(kind=c_int)::res
			integer(kind=c_int8_t)::passwd
			integer(kind=c_size_t)::passwdlen
			integer(kind=c_int8_t)::salt
			integer(kind=c_size_t),value::saltlen
			integer(kind=c_int64_t)::n
			integer(kind=c_int32_t)::r
			integer(kind=c_int32_t)::p
			integer(kind=c_int8_t)::buf
			integer(kind=c_size_t),value::buflen
		endfunction crypto_pwhash_scryptsalsa208sha256_ll

		function crypto_pwhash_scryptsalsa208sha256_str_needs_rehash(str,opslimit,memlimit)&
		&bind(c,name='crypto_pwhash_scryptsalsa208sha256_str_needs_rehash')&
		&result(res)
			import::c_char,c_int,c_long_long,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::str
			integer(kind=c_size_t),value::memlimit
			integer(kind=c_long_long),value::opslimit
		endfunction crypto_pwhash_scryptsalsa208sha256_str_needs_rehash

	endinterface

	contains

		function crypto_pwhash_scryptsalsa208sha256_strprefix()result(res)
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_pwhash_scryptsalsa208sha256_strprefix()
			call c_f_str_ptr(res1,res)
		endfunction crypto_pwhash_scryptsalsa208sha256_strprefix

endmodule mod_crypto_pwhash_scryptsalsa208sha256