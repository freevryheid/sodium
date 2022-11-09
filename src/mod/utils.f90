module mod_utils

	use,intrinsic::iso_c_binding
	use::mod_common

	implicit none

	private

	public::sodium_memzero
	public::sodium_stackzero
	public::sodium_memcmp
	public::sodium_compare
	public::sodium_is_zero
	public::sodium_increment
	public::sodium_add
	public::sodium_sub
	public::sodium_bin2hex
	public::sodium_hex2bin
	public::sodium_base64_encoded_len
	public::sodium_bin2base64
	public::sodium_base642bin
	public::sodium_mlock
	public::sodium_munlock
	public::sodium_malloc
	public::sodium_allocarray
	public::sodium_free
	public::sodium_mprotect_noaccess
	public::sodium_mprotect_readonly
	public::sodium_mprotect_readwrite
	public::sodium_pad
	public::sodium_unpad

	integer,parameter,public::SODIUM_BASE64_VARIANT_ORIGINAL=1
	integer,parameter,public::SODIUM_BASE64_VARIANT_ORIGINAL_NO_PADDING=3
	integer,parameter,public::SODIUM_BASE64_VARIANT_URLSAFE=5
	integer,parameter,public::SODIUM_BASE64_VARIANT_URLSAFE_NO_PADDING=7

	interface

		subroutine sodium_memzero(pnt,len)bind(c,name='sodium_memzero')
			import::c_char,c_size_t
			character(kind=c_char)::pnt
			integer(kind=c_size_t),value::len
		endsubroutine sodium_memzero

		subroutine sodium_stackzero(len)bind(c,name='sodium_stackzero')
			import::c_size_t
			integer(kind=c_size_t),value::len
		endsubroutine sodium_stackzero

		function sodium_memcmp(b1_,b2_,len)bind(c,name='sodium_memcmp')result(res)
			import::c_int,c_size_t,c_char
			integer(kind=c_int)::res
			character(kind=c_char)::b1_
			character(kind=c_char)::b2_
			integer(kind=c_size_t)::len ! CHK:value
		endfunction sodium_memcmp

		function sodium_compare(b1_,b2_,len)bind(c,name='sodium_compare')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::b1_
			character(kind=c_char)::b2_
			integer(kind=c_size_t),value::len
		endfunction sodium_compare

		function sodium_is_zero(n,nlen)bind(c,name='sodium_is_zero')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::n
			integer(kind=c_size_t),value::nlen
		endfunction sodium_is_zero

		subroutine sodium_increment(n,nlen)bind(c,name='sodium_increment')
			import::c_char,c_size_t
			character(kind=c_char)::n
			integer(kind=c_size_t),value::nlen
		endsubroutine sodium_increment

		subroutine sodium_add(a,b,len)bind(c,name='sodium_add')
			import::c_char,c_size_t
			character(kind=c_char)::a
			character(kind=c_char)::b
			integer(kind=c_size_t),value::len
		endsubroutine sodium_add

		subroutine sodium_sub(a,b,len)bind(c,name='sodium_sub')
			import::c_char,c_size_t
			character(kind=c_char)::a
			character(kind=c_char)::b
			integer(kind=c_size_t),value::len
		endsubroutine sodium_sub

		function bind_sodium_bin2hex(hex,hex_maxlen,bin,bin_len)&
		&bind(c,name='sodium_bin2hex')result(res)
			import::c_char,c_ptr,c_size_t
			type(c_ptr)::res
			character(kind=c_char)::hex
			integer(kind=c_size_t),value::hex_maxlen
			character(kind=c_char)::bin
			integer(kind=c_size_t),value::bin_len
		endfunction bind_sodium_bin2hex

		function sodium_hex2bin(bin,bin_maxlen,hex,hex_len,ignore,bin_len,hex_end)&
		&bind(c,name='sodium_hex2bin')result(res)
			import::c_char,c_int,c_ptr,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::bin
			integer(kind=c_size_t),value::bin_maxlen
			character(kind=c_char)::hex
			integer(kind=c_size_t),value::hex_len
			character(kind=c_char)::ignore
			integer(kind=c_size_t)::bin_len
			type(c_ptr)::hex_end
		endfunction sodium_hex2bin

		function sodium_base64_encoded_len(bin_len,variant)&
		&bind(c,name='sodium_base64_encoded_len')result(res)
			import::c_int,c_size_t
			integer(kind=c_size_t)::res
			integer(kind=c_size_t),value::bin_len
			integer(kind=c_int),value::variant
		endfunction sodium_base64_encoded_len

		function bind_sodium_bin2base64(b64,b64_maxlen,bin,bin_len,variant)&
		&bind(c,name='sodium_bin2base64')result(res)
			import::c_char,c_int,c_ptr,c_size_t
			type(c_ptr)::res
			character(kind=c_char)::b64
			integer(kind=c_size_t),value::b64_maxlen
			character(kind=c_char)::bin
			integer(kind=c_size_t),value::bin_len
			integer(kind=c_int),value::variant
		endfunction bind_sodium_bin2base64

		function sodium_base642bin(bin,bin_maxlen,b64,b64_len,ignore,bin_len,b64_end,variant)&
		&bind(c,name='sodium_base642bin')result(res)
			import::c_char,c_int,c_ptr,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::bin
			integer(kind=c_size_t),value::bin_maxlen
			character(kind=c_char)::b64
			integer(kind=c_size_t),value::b64_len
			character(kind=c_char)::ignore
			integer(kind=c_size_t)::bin_len
			type(c_ptr)::b64_end
			integer(kind=c_int),value::variant
		endfunction sodium_base642bin

		function sodium_mlock(addr,len)bind(c,name='sodium_mlock')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::addr
			integer(kind=c_size_t),value::len
		endfunction sodium_mlock

		function sodium_munlock(addr,len)bind(c,name='sodium_munlock')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			character(kind=c_char)::addr
			integer(kind=c_size_t),value::len
		endfunction sodium_munlock

		function sodium_malloc(size)bind(c,name='sodium_malloc')result(res)
			import::c_char,c_size_t
			integer(kind=c_size_t),value::size
			character(kind=c_char)::res
		endfunction sodium_malloc

		! TODO: convert to fortran pointer
		function sodium_allocarray(count,size)bind(c,name='sodium_allocarray')result(res)
			import::c_size_t,c_ptr
			integer(kind=c_size_t)::count
			integer(kind=c_size_t)::size
			type(c_ptr)::res
		endfunction sodium_allocarray

		subroutine sodium_free(ptr)bind(c,name='sodium_free')
			import::c_ptr
			type(c_ptr)::ptr
		endsubroutine sodium_free

		function sodium_mprotect_noaccess(ptr)bind(c,name='sodium_mprotect_noaccess')result(res)
			import::c_int,c_ptr
			integer(kind=c_int)::res
			type(c_ptr)::ptr
		endfunction sodium_mprotect_noaccess

		function sodium_mprotect_readonly(ptr)bind(c,name='sodium_mprotect_readonly')result(res)
			import::c_int,c_ptr
			integer(kind=c_int)::res
			type(c_ptr)::ptr
		endfunction sodium_mprotect_readonly

		function sodium_mprotect_readwrite(ptr)bind(c,name='sodium_mprotect_readwrite')result(res)
			import::c_int,c_ptr
			integer(kind=c_int)::res
			type(c_ptr)::ptr
		endfunction sodium_mprotect_readwrite

		function sodium_pad(padded_buflen_p,buf,unpadded_buflen,blocksize,max_buflen)&
		&bind(c,name='sodium_pad')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			integer(kind=c_size_t)::padded_buflen_p
			character(kind=c_char)::buf
			integer(kind=c_size_t),value::unpadded_buflen
			integer(kind=c_size_t),value::blocksize
			integer(kind=c_size_t),value::max_buflen
		endfunction sodium_pad

		function sodium_unpad(unpadded_buflen_p,buf,padded_buflen,blocksize)&
		&bind(c,name='sodium_unpad')result(res)
			import::c_char,c_int,c_size_t
			integer(kind=c_int)::res
			integer(kind=c_size_t)::unpadded_buflen_p
			character(kind=c_char)::buf
			integer(kind=c_size_t),value::padded_buflen
			integer(kind=c_size_t),value::blocksize
		endfunction sodium_unpad

	endinterface

	contains

		function sodium_bin2hex(hex,hex_maxlen,bin,bin_len)result(res)
			character(len=:),allocatable::hex
			character(len=:),allocatable::bin
			character(len=:),allocatable::res
			integer(kind=c_size_t)::hex_maxlen,bin_len
			type(c_ptr)::res1
			res1=bind_sodium_bin2hex(hex,hex_maxlen,bin,bin_len)
			allocate(character(len=hex_maxlen)::res)
			call c_f_str_ptr(res1,res)
		endfunction sodium_bin2hex

		function sodium_bin2base64(b64,b64_maxlen,bin,bin_len,variant)result(res)
			character(len=:),allocatable::b64
			character(len=:),allocatable::bin
			character(len=:),allocatable::res
			integer(kind=c_size_t)::b64_maxlen,bin_len
			integer(kind=c_int)::variant
			type(c_ptr)::res1
			res1=bind_sodium_bin2base64(b64,b64_maxlen,bin,bin_len,variant)
			allocate(character(len=b64_maxlen)::res)
			call c_f_str_ptr(res1,res)
		endfunction sodium_bin2base64

endmodule mod_utils