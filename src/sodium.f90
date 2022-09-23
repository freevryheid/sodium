module sodium

	use, intrinsic :: iso_c_binding
	use stdlib_kinds
	! use fortium

	implicit none
  ! character(len=1,kind=C_char), parameter :: NUL = C_NULL_char
	private

	! type(c_ptr) :: x
	! logical, parameter, public :: IS64 = c_sizeof(x) == 8
	integer, parameter, public :: SODIUM_BASE64_VARIANT_ORIGINAL = 1
	integer, parameter, public :: SODIUM_BASE64_VARIANT_ORIGINAL_NO_PADDING = 3
	integer, parameter, public :: SODIUM_BASE64_VARIANT_URLSAFE = 5
	integer, parameter, public :: SODIUM_BASE64_VARIANT_URLSAFE_NO_PADDING = 7

	public :: sodium_init
	public :: sodium_memcmp
	public :: sodium_bin2hex
	public :: sodium_hex2bin
	public :: sodium_base64_encoded_len
	public :: sodium_bin2base64
	public :: sodium_base642bin
	public :: randombytes_random
	public :: randombytes_uniform
	public :: randombytes_buf
	public :: crypto_secretbox_keybytes
	public :: crypto_secretbox_noncebytes
	public :: crypto_secretbox_macbytes
	public :: crypto_secretbox_keygen
	public :: crypto_secretbox_easy
	public :: crypto_secretbox_open_easy
	public :: c_str

	interface

		function c_strlen(str) bind(c, name='strlen')
			import :: c_ptr, c_size_t
			implicit none
			type(c_ptr), intent(in), value :: str
			integer(c_size_t) :: c_strlen
		end function c_strlen

		! int sodium_init(void)
		function sodium_init() bind(c, name="sodium_init") result(res)
			!! initializes the library and should be called before any other function provided by
			!! Sodium. It is safe to call this function more than once and from different threads,
			!! subsequent calls won't have any effects.
			!! After this function returns, all of the other functions provided by Sodium will be thread-safe.
			!! returns 0 on success, -1 on failure, and 1 if the library had already been initialized.
			!! Before returning, the function ensures that the system's random number generator has been properly seeded.
			import :: c_int
			integer(kind=c_int) :: res
		end function sodium_init

		! int sodium_memcmp(const void * const b1_, const void * const b2_, size_t len)
		function s_memcmp(b1, b2, siz) bind(c, name="sodium_memcmp") result (res)
			import :: c_int, c_char, c_size_t
			character(kind=c_char), intent(in) :: b1, b2
			integer(kind=c_size_t), value, intent(in) :: siz
			integer(kind=c_int) :: res
		end function s_memcmp

		! char *sodium_bin2hex(char * const hex, const size_t hex_maxlen,
		!                      const unsigned char * const bin, const size_t bin_len)
		function s_bin2hex(hex, hex_maxlen, bin, bin_len) bind(c, name="sodium_bin2hex") result(res)
			import :: c_int, c_char, c_size_t, c_ptr
			character(kind=c_char), intent(inout) :: hex
			character(kind=c_char), intent(in) :: bin
			integer(kind=c_size_t), value, intent(in) :: hex_maxlen, bin_len
			type(c_ptr) :: res
		end function s_bin2hex

		! int sodium_hex2bin(unsigned char * const bin, const size_t bin_maxlen,
		!                    const char * const hex, const size_t hex_len,
		!                    const char * const ignore, size_t * const bin_len,
		!                    const char ** const hex_end)
		function s_hex2bin(bin, bin_maxlen, hex, hex_len, ignore, bin_len, hex_end) bind(c, name="sodium_hex2bin") result(res)
			import :: c_int, c_char, c_size_t, c_ptr
			character(kind=c_char), intent(inout) :: bin
			character(kind=c_char), intent(in) :: hex, ignore
			integer(kind=c_size_t), value, intent(in) :: bin_maxlen, hex_len
			integer(kind=c_size_t), intent(inout) :: bin_len
			type(c_ptr), intent(inout) :: hex_end
			integer(kind=c_int) :: res
		end function s_hex2bin

		! size_t sodium_base64_encoded_len(const size_t bin_len, const int variant)
		function sodium_base64_encoded_len(bin_len, variant) bind(c, name="sodium_base64_encoded_len") result(res)
			!! returns the minimum number of bytes required to encode BIN_LEN bytes using the Base64 variant VARIANT.
			!! The returned length includes a trailing \0 byte (TODO: confirm this).
			import :: c_size_t, c_int
			integer(kind=c_size_t), value, intent(in) :: bin_len
			integer(kind=c_int), value, intent(in) :: variant
			integer(kind=c_size_t) :: res
		end function sodium_base64_encoded_len

		! char *sodium_bin2base64(char * const b64, const size_t b64_maxlen,
		!                         const unsigned char * const bin, const size_t bin_len,
		!                         const int variant)
		function s_bin2base64(b64, b64_maxlen, bin, bin_len, variant) bind(c, name="sodium_bin2base64") result(res)
			import :: c_int, c_char, c_size_t, c_ptr
			character(kind=c_char), intent(inout) :: b64
			character(kind=c_char), intent(in) :: bin
			integer(kind=c_size_t), value, intent(in) :: b64_maxlen, bin_len
			integer(kind=c_int), value, intent(in) :: variant
			type(c_ptr) :: res
		end function s_bin2base64

		! int sodium_base642bin(unsigned char * const bin, const size_t bin_maxlen,
		!                       const char * const b64, const size_t b64_len,
		!                       const char * const ignore, size_t * const bin_len,
		!                       const char ** const b64_end, const int variant);
		function s_base642bin(bin, bin_maxlen, b64, b64_len, ignore, bin_len, b64_end, variant) &
		& bind(c, name="sodium_base642bin") result(res)
			import :: c_int, c_char, c_size_t, c_ptr
			character(kind=c_char), intent(inout) :: bin
			character(kind=c_char), intent(in) :: b64, ignore
			integer(kind=c_size_t), value, intent(in) :: bin_maxlen, b64_len
			integer(kind=c_size_t), intent(inout) :: bin_len
			type(c_ptr), intent(inout) :: b64_end
			integer(kind=c_int), value, intent(in) :: variant
			integer(kind=c_int) :: res
		end function s_base642bin

		! size_t  crypto_secretbox_keybytes(void)
		function crypto_secretbox_keybytes() bind(c, name="crypto_secretbox_keybytes") result(res)
			import :: c_size_t
			integer(kind=c_size_t) :: res
		end function crypto_secretbox_keybytes

		! size_t  crypto_secretbox_noncebytes(void)
		function crypto_secretbox_noncebytes() bind(c, name="crypto_secretbox_noncebytes") result(res)
			import :: c_size_t
			integer(kind=c_size_t) :: res
		end function crypto_secretbox_noncebytes

		! size_t  crypto_secretbox_macbytes(void)
		function crypto_secretbox_macbytes() bind(c, name="crypto_secretbox_macbytes") result(res)
			import :: c_size_t
			integer(kind=c_size_t) :: res
		end function crypto_secretbox_macbytes

		! uint32_t randombytes_random(void);
		function rb_random() bind(c, name="randombytes_random") result(res)
		  ! using signed int64 to hold unsigned int32
			import :: c_int64_t
			integer(kind=c_int64_t) :: res
		end function rb_random

		! uint32_t randombytes_uniform(const uint32_t upper_bound)
		function rb_uniform(upper_bound) bind(c, name="randombytes_uniform") result(res)
			import :: c_int64_t
			integer(kind=c_int64_t), value, intent(in) :: upper_bound
			integer(kind=c_int64_t) :: res
		end function rb_uniform

		! void randombytes_buf(void * const buf, const size_t size)
		subroutine rb_buf(buf, siz) bind(c, name="randombytes_buf")
			import :: c_char, c_size_t
			character(kind=c_char), intent(inout) :: buf
			integer(kind=c_size_t), value, intent(in) :: siz
		end subroutine rb_buf

		! void crypto_secretbox_keygen(unsigned char k[crypto_secretbox_KEYBYTES])
		subroutine c_secretbox_keygen(key) bind(c, name="crypto_secretbox_keygen")
			import :: c_char
			character(kind=c_char), intent(inout) :: key
		end subroutine c_secretbox_keygen

		! int crypto_secretbox_easy(unsigned char *c, const unsigned char *m,
		!                           unsigned long long mlen, const unsigned char *n,
		!                           const unsigned char *k);
		function c_secretbox_easy(c, m, mlen, n, k) bind(c, name="crypto_secretbox_easy") result(res)
			import :: c_char, c_long_long, c_int
			character(kind=c_char), intent(inout) :: c
			character(kind=c_char), intent(in) :: m, n, k
			integer(kind=c_long_long), value, intent(in) :: mlen
			integer(kind=c_int) :: res
		end function c_secretbox_easy

		! int crypto_secretbox_open_easy(unsigned char *m, const unsigned char *c,
		!                                unsigned long long clen, const unsigned char *n,
		!                                const unsigned char *k);
		function c_secretbox_open_easy(m, c, clen, n, k) bind(c, name="crypto_secretbox_open_easy") result(res)
			import :: c_char, c_long_long, c_int
			character(kind=c_char), intent(inout) :: m
			character(kind=c_char), intent(in) :: c, n, k
			integer(kind=c_long_long), value, intent(in) :: clen
			integer(kind=c_int) :: res
		end function c_secretbox_open_easy

	end interface

	contains

		pure function copy(a)
			character, intent(in)  :: a(:)
			character(len=size(a)) :: copy
			integer(kind=8) :: i
			do i = 1, size(a)
				copy(i:i) = a(i)
			end do
		end function copy

		subroutine c_f_str_ptr(cstr, fstr)
			!! Extract fortran string from a c pointer.
			type(c_ptr), intent(in) :: cstr
			character(len=:), allocatable, intent(out) :: fstr
			character(kind=c_char), pointer :: ptrs(:)
			integer(kind=8) :: sz
			if (.not. c_associated(cstr)) return
			sz = c_strlen(cstr)
			if (sz < 0) return
			call c_f_pointer(cstr, ptrs, [ sz ])
			allocate (character(len=sz) :: fstr)
			fstr = copy(ptrs)
		end subroutine c_f_str_ptr

		function c_str(fstr) result(cstr)
			!! Converts fortran string to null terminated c string.
			character(len=*), intent(in) :: fstr
				!! Fortran string.
			character(kind=c_char, len=:), allocatable :: cstr
				!! NULL terminated string.
			cstr = trim(fstr) // c_null_char
		end function c_str

		function sodium_memcmp(b1, b2, siz) result(res)
			!! When a comparison involves secret data (e.g. a key, an authentication tag, etc), it is critical
			!! to use a constant-time comparison function. This property does not relate to computational
			!! complexity: it means the time needed to perform the comparison is the same for all data of
			!! the same size. The goal is to mitigate side-channel attacks.
			!! The sodium_memcmp() function can be used for this purpose.
			!! The function returns 0 if the len bytes pointed to by b1 match the len bytes pointed to by
			!! b2. Otherwise, it returns -1.
			character(len=:), allocatable, intent(in) :: b1, b2
			integer(kind=c_size_t), intent(in) :: siz
			integer :: res
			res = s_memcmp(b1, b2, siz)
		end function sodium_memcmp

		function sodium_bin2hex(hex, hex_maxlen, bin, bin_len) result(res)
			!! converts bin_len bytes stored at bin into a hexadecimal string.
			!! The string is stored into hex and includes a nul byte (\0) terminator.
			!! hex_maxlen is the maximum number of bytes that the function is allowed to write starting at hex.
			!! It must be at least bin_len * 2 + 1 bytes.
			!! The function always returns hex. It evaluates in constant time for a given size.
			character(len=:), allocatable, intent(inout) :: hex
			character(len=:), allocatable, intent(in) :: bin
			character(len=:), allocatable :: res
			integer(kind=c_size_t), intent(in) :: hex_maxlen, bin_len
			type(c_ptr) :: res1
			res1 = s_bin2hex(hex, hex_maxlen, bin, bin_len)
			allocate(character(len=hex_maxlen) :: res)
			call c_f_str_ptr(res1, res)
		end function sodium_bin2hex

		function sodium_bin2base64(b64, b64_maxlen, bin, bin_len, variant) result(res)
			!! encodes bin as a Base64 string. variant must be one of:
			!! SODIUM_BASE64_VARIANT_ORIGINAL
			!! SODIUM_BASE64_VARIANT_ORIGINAL_NO_PADDING
			!! SODIUM_BASE64_VARIANT_URLSAFE
			!! SODIUM_BASE64_VARIANT_URLSAFE_NO_PADDING
			!! None of these Base64 variants provides any form of encryption; just like hex encoding, anyone can decode them.
			!! Computing a correct size for b64_maxlen is not straightforward and depends on the chosen variant.
			!! sodium_base64_encoded_len(size_t bin_len, int variant) function is available for this purpose.
			character(len=:), allocatable, intent(inout) :: b64
			character(len=:), allocatable, intent(in) :: bin
			character(len=:), allocatable :: res
			integer(kind=c_size_t), intent(in) :: b64_maxlen, bin_len
			integer(kind=c_int), intent(in) :: variant
			type(c_ptr) :: res1
			res1 = s_bin2base64(b64, b64_maxlen, bin, bin_len, variant)
			allocate(character(len=b64_maxlen) :: res)
			call c_f_str_ptr(res1, res)
		end function sodium_bin2base64

		function sodium_hex2bin(bin, bin_maxlen, hex, hex_len, ignore, bin_len, hex_end) result(res)
			!! parses a hexadecimal string hex and converts it to a byte sequence.
			!! hex does not have to be nul terminated, as the number of characters to parse is supplied via the hex_len parameter.
			!! ignore is a string of characters to skip. For example, the string ": " allows colons and spaces to be present at any
			!! location in the hexadecimal string. These characters will just be ignored.
			!! As a result, "69:FC", "69 FC", "69 : FC" and "69FC" will be valid inputs and produce the same output.
			!! ignore can be set to NULL to disallow any non-hexadecimal character.
			!! bin_maxlen is the maximum number of bytes to put into bin.
			!! The parser stops when a non-hexadecimal, non-ignored character is found or when bin_maxlen bytes have been written.
			!! If hex_end is not NULL, it will be set to the address of the first byte after the last valid parsed character.
			!! The function returns 0 on success.
			!! It returns -1 if more than bin_maxlen bytes would be required to store the parsed string or the string couldn't be
			!! fully parsed, but a valid pointer for hex_end was not provided.
			!! It evaluates in constant time for a given length and format.
			character(len=:), allocatable, intent(inout) :: bin
			character(len=:), allocatable, intent(in) :: hex, ignore
			integer(kind=c_size_t), intent(in) :: bin_maxlen, hex_len
			integer(kind=c_size_t), intent(inout) :: bin_len
			type(c_ptr), intent(inout) :: hex_end
			integer(kind=c_int) :: res
			res = s_hex2bin(bin, bin_maxlen, hex, hex_len, ignore, bin_len, hex_end)
		end function sodium_hex2bin

		function sodium_base642bin(bin, bin_maxlen, b64, b64_len, ignore, bin_len, b64_end, variant) result(res)
			!! decodes a Base64 string using the given variant and an optional set of characters to ignore (typically: whitespaces and newlines).
			!! If b64_end is not NULL, it will be set to the address of the first byte after the last valid parsed character.
			!! Base64 encodes 3 bytes as 4 characters, so the result of decoding a b64_len string will always be at most b64_len / 4 * 3 bytes long.
			!! The function returns 0 on success.
			!! It returns -1 if more than bin_maxlen bytes would be required to store the parsed string or the string couldn't be fully parsed,
			!! but a valid pointer for b64_end was not provided.
			character(len=:), allocatable, intent(inout) :: bin
			character(len=:), allocatable, intent(in) :: b64, ignore
			integer(kind=c_size_t), intent(in) :: bin_maxlen, b64_len
			integer(kind=c_size_t), intent(inout) :: bin_len
			type(c_ptr), intent(inout) :: b64_end
			integer(kind=c_int), intent(in) :: variant
			integer(kind=c_int) :: res
			res = s_base642bin(bin, bin_maxlen, b64, b64_len, ignore, bin_len, b64_end, variant)
		end function sodium_base642bin

		function randombytes_random() result(res)
			!! returns an unpredictable value between 0 and 0xffffffff (included)
			integer(kind=int64) :: res
			res = int(rb_random(), kind=int64)
		end function randombytes_random

		function randombytes_uniform(upper_bound) result(res)
			!! returns an unpredictable value between 0 and upper_bound (excluded).
			!! Unlike randombytes_random() % upper_bound, it guarantees a uniform
			!! distribution of the possible output values even when upper_bound is
			!! not a power of 2.
			integer(kind=int64), intent(in) :: upper_bound
			integer(kind=int64) :: res
			res = int(rb_uniform(upper_bound), kind=int64)
		end function randombytes_uniform

		subroutine randombytes_buf(buf, siz)
			!! fills size bytes starting at buf with an unpredictable sequence of bytes
			character(len=:), allocatable, intent(inout) :: buf
			integer(kind=c_size_t), intent(in) :: siz
			call rb_buf(buf, siz)
		end subroutine randombytes_buf

		function crypto_secretbox_keygen() result(res)
			!! creates a random key
			character(len=:), allocatable :: res
			character(len=:), allocatable :: key
			integer(c_size_t) :: klen
			klen = crypto_secretbox_keybytes()
			allocate(character(len=klen) :: key)
			call c_secretbox_keygen(key)
			res = key
		end function crypto_secretbox_keygen

		function crypto_secretbox_easy(c, m, mlen, n, k) result(res)
			!! encrypts a message m whose length is mlen bytes, with a key k and a nonce n.
			!! k should be crypto_secretbox_KEYBYTES bytes and n should be crypto_secretbox_NONCEBYTES bytes.
			!! c should be at least crypto_secretbox_MACBYTES + mlen bytes long.
			!! This function writes the authentication tag, whose length is crypto_secretbox_MACBYTES bytes,
			!! in c, immediately followed by the encrypted message, whose length is the same as the plaintext: mlen.
			!! c and m can overlap, making in-place encryption possible. However do not forget that
			!! crypto_secretbox_MACBYTES extra bytes are required to prepend the tag.
			character(len=:), allocatable, intent(inout) :: c
			character(len=:), allocatable, intent(in) :: m, n, k
			integer(kind=c_long_long), intent(in) :: mlen
			integer :: res
			res = int(c_secretbox_easy(c, m, mlen, n, k))
		end function crypto_secretbox_easy

		function crypto_secretbox_open_easy(m, c, clen, n, k) result(res)
			!! verifies and decrypts a ciphertext produced by crypto_secretbox_easy().
			!! c is a pointer to an authentication tag + encrypted message combination, as produced by crypto_secretbox_easy(). clen is the length of this authentication tag + encrypted message combination. Put differently, clen is the number of bytes written by crypto_secretbox_easy(), which is crypto_secretbox_MACBYTES + the length of the message.
			!! The nonce n and the key k have to match those used to encrypt and authenticate the message.
			!! The function returns -1 if the verification fails, and 0 on success. On success, the decrypted message is stored into m.
			!! m and c can overlap, making in-place decryption possible.
			character(len=:), allocatable, intent(inout) :: m
			character(len=:), allocatable, intent(in) :: c, n, k
			integer(kind=c_long_long), intent(in) :: clen
			integer :: res
			res = int(c_secretbox_open_easy(m, c, clen, n, k))
		end function crypto_secretbox_open_easy

end module sodium
