module mod_crypto_pwhash

	use,intrinsic::iso_c_binding
	! use::mod_crypto_generichash_blake2b
	use::mod_common

	implicit none

	private

	public::crypto_pwhash_alg_argon2i13
	public::crypto_pwhash_alg_argon2id13
	public::crypto_pwhash_alg_default
	public::crypto_pwhash_bytes_min
	public::crypto_pwhash_bytes_max
	public::crypto_pwhash_passwd_min
	public::crypto_pwhash_passwd_max
	public::crypto_pwhash_saltbytes
	public::crypto_pwhash_strbytes
	public::crypto_pwhash_strprefix
	public::crypto_pwhash_opslimit_min
	public::crypto_pwhash_opslimit_max
	public::crypto_pwhash_memlimit_min
	public::crypto_pwhash_memlimit_max
	public::crypto_pwhash_opslimit_interactive
	public::crypto_pwhash_memlimit_interactive
	public::crypto_pwhash_opslimit_moderate
	public::crypto_pwhash_memlimit_moderate
	public::crypto_pwhash_opslimit_sensitive
	public::crypto_pwhash_memlimit_sensitive
	public::crypto_pwhash
	public::crypto_pwhash_str
	public::crypto_pwhash_str_alg
	public::crypto_pwhash_str_verify
	public::crypto_pwhash_str_needs_rehash
	public::crypto_pwhash_primitive


 ! #define crypto_pwhash_ALG_ARGON2I13 crypto_pwhash_argon2i_ALG_ARGON2I13
 ! #define crypto_pwhash_ALG_ARGON2ID13 crypto_pwhash_argon2id_ALG_ARGON2ID13
 ! #define crypto_pwhash_ALG_DEFAULT crypto_pwhash_ALG_ARGON2ID13
 ! #define crypto_pwhash_BYTES_MIN crypto_pwhash_argon2id_BYTES_MIN
 ! #define crypto_pwhash_BYTES_MAX crypto_pwhash_argon2id_BYTES_MAX
 ! #define crypto_pwhash_PASSWD_MIN crypto_pwhash_argon2id_PASSWD_MIN
 ! #define crypto_pwhash_PASSWD_MAX crypto_pwhash_argon2id_PASSWD_MAX
 ! #define crypto_pwhash_SALTBYTES crypto_pwhash_argon2id_SALTBYTES
 ! #define crypto_pwhash_STRBYTES crypto_pwhash_argon2id_STRBYTES
 ! #define crypto_pwhash_STRPREFIX crypto_pwhash_argon2id_STRPREFIX
 ! #define crypto_pwhash_OPSLIMIT_MIN crypto_pwhash_argon2id_OPSLIMIT_MIN
 ! #define crypto_pwhash_OPSLIMIT_MAX crypto_pwhash_argon2id_OPSLIMIT_MAX
 ! #define crypto_pwhash_MEMLIMIT_MIN crypto_pwhash_argon2id_MEMLIMIT_MIN
 ! #define crypto_pwhash_MEMLIMIT_MAX crypto_pwhash_argon2id_MEMLIMIT_MAX
 ! #define crypto_pwhash_OPSLIMIT_INTERACTIVE crypto_pwhash_argon2id_OPSLIMIT_INTERACTIVE
 ! #define crypto_pwhash_MEMLIMIT_INTERACTIVE crypto_pwhash_argon2id_MEMLIMIT_INTERACTIVE
 ! #define crypto_pwhash_OPSLIMIT_MODERATE crypto_pwhash_argon2id_OPSLIMIT_MODERATE
 ! #define crypto_pwhash_MEMLIMIT_MODERATE crypto_pwhash_argon2id_MEMLIMIT_MODERATE
 ! #define crypto_pwhash_OPSLIMIT_SENSITIVE crypto_pwhash_argon2id_OPSLIMIT_SENSITIVE
 ! #define crypto_pwhash_MEMLIMIT_SENSITIVE crypto_pwhash_argon2id_MEMLIMIT_SENSITIVE
 ! #define crypto_pwhash_PRIMITIVE "argon2i"


	interface

		function crypto_pwhash_alg_argon2i13(void)bind(c,name='crypto_pwhash_alg_argon2i13')result(res)
			import::c_int
			integer(kind=c_int)::res
		end function crypto_pwhash_alg_argon2i13

		function crypto_pwhash_alg_argon2id13(void)bind(c,name='crypto_pwhash_alg_argon2id13')result(res)
			import::c_int
			integer(kind=c_int)::res
		end function crypto_pwhash_alg_argon2id13

		function crypto_pwhash_alg_default(void)bind(c,name='crypto_pwhash_alg_default')result(res)
			import::c_int
			integer(kind=c_int)::res
		end function crypto_pwhash_alg_default

		function crypto_pwhash_bytes_min(void)bind(c,name='crypto_pwhash_bytes_min')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_bytes_min

		function crypto_pwhash_bytes_max(void)bind(c,name='crypto_pwhash_bytes_max')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_bytes_max

		function crypto_pwhash_passwd_min(void)bind(c,name='crypto_pwhash_passwd_min')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_passwd_min

		function crypto_pwhash_passwd_max(void)bind(c,name='crypto_pwhash_passwd_max')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_passwd_max

		function crypto_pwhash_saltbytes(void)bind(c,name='crypto_pwhash_saltbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_saltbytes

		function crypto_pwhash_strbytes(void)bind(c,name='crypto_pwhash_strbytes')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_strbytes

		function bind_crypto_pwhash_strprefix(void)bind(c,name='crypto_pwhash_strprefix')result(res)
			import::c_ptr
			type(c_ptr)::res
		end function bind_crypto_pwhash_strprefix

		function crypto_pwhash_opslimit_min(void)bind(c,name='crypto_pwhash_opslimit_min')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_opslimit_min

		function crypto_pwhash_opslimit_max(void)bind(c,name='crypto_pwhash_opslimit_max')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_opslimit_max

		function crypto_pwhash_memlimit_min(void)bind(c,name='crypto_pwhash_memlimit_min')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_memlimit_min

		function crypto_pwhash_memlimit_max(void)bind(c,name='crypto_pwhash_memlimit_max')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_memlimit_max

		function crypto_pwhash_opslimit_interactive(void)bind(c,name='crypto_pwhash_opslimit_interactive')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_opslimit_interactive

		function crypto_pwhash_memlimit_interactive(void)bind(c,name='crypto_pwhash_memlimit_interactive')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_memlimit_interactive

		function crypto_pwhash_opslimit_moderate(void)bind(c,name='crypto_pwhash_opslimit_moderate')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_opslimit_moderate

		function crypto_pwhash_memlimit_moderate(void)bind(c,name='crypto_pwhash_memlimit_moderate')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_memlimit_moderate

		function crypto_pwhash_opslimit_sensitive(void)bind(c,name='crypto_pwhash_opslimit_sensitive')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_opslimit_sensitive

		function crypto_pwhash_memlimit_sensitive(void)bind(c,name='crypto_pwhash_memlimit_sensitive')result(res)
			import::c_size_t
			integer(kind=c_size_t)::res
		end function crypto_pwhash_memlimit_sensitive



 ! int
		function crypto_pwhash(out,outlen,passwd,passwdlen,salt,opslimit,memlimit,alg)bind(c,name='crypto_pwhash')result(res)

		end function crypto_pwhash

 ! unsigned char * const :: out
 ! unsigned long long :: outlen
 ! const char * const :: passwd
 ! unsigned long long :: passwdlen
 ! const unsigned char * const :: salt
 ! unsigned long long :: opslimit
 ! size_t :: memlimit
 ! int :: alg
 !---
 ! int
		function crypto_pwhash_str(out,passwd,passwdlen,opslimit,memlimit)bind(c,name='crypto_pwhash_str')result(res)

		end function crypto_pwhash_str

 ! char :: out
 ! const char * const :: passwd
 ! unsigned long long :: passwdlen
 ! unsigned long long :: opslimit
 ! size_t :: memlimit
 !---
 ! int
		function crypto_pwhash_str_alg(out,passwd,passwdlen,opslimit,memlimit,alg)bind(c,name='crypto_pwhash_str_alg')result(res)

		end function crypto_pwhash_str_alg

 ! char :: out
 ! const char * const :: passwd
 ! unsigned long long :: passwdlen
 ! unsigned long long :: opslimit
 ! size_t :: memlimit
 ! int :: alg
 !---
 ! int
		function crypto_pwhash_str_verify(str,passwd,passwdlen)bind(c,name='crypto_pwhash_str_verify')result(res)

		end function crypto_pwhash_str_verify

 ! const char :: str
 ! const char * const :: passwd
 ! unsigned long long :: passwdlen
 !---
 ! int
		function crypto_pwhash_str_needs_rehash(str,opslimit,memlimit)bind(c,name='crypto_pwhash_str_needs_rehash')result(res)

		end function crypto_pwhash_str_needs_rehash

 ! const char :: str
 ! unsigned long long :: opslimit
 ! size_t :: memlimit






		function crypto_pwhash_primitive(void)bind(c,name='crypto_pwhash_primitive')result(res)
			import::c_ptr
			type(c_ptr)::res
		end function crypto_pwhash_primitive

	endinterface

	contains

		function crypto_pwhash_strprefix()
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_pwhash_strprefix()
			call c_f_str_ptr(res1,res)
		end function crypto_pwhash_strprefix

		function crypto_pwhash_primitive()
			type(c_ptr)::res1
			character(len=:),allocatable::res
			res1=bind_crypto_pwhash_primitive()
			call c_f_str_ptr(res1,res)
		end function crypto_pwhash_primitive

endmodule mod_crypto_pwhash

