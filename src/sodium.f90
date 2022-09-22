module sodium

	use, intrinsic :: iso_c_binding
	use stdlib_kinds
	! use fortium

	implicit none
  ! character(len=1,kind=C_char), parameter :: NUL = C_NULL_char
	private

	! type(c_ptr) :: x
	! logical, parameter, public :: IS64 = c_sizeof(x) == 8

	public :: sodium_init
	public :: sodium_memcmp
	public :: randombytes_random
	public :: randombytes_uniform
	public :: randombytes_buf
	public :: crypto_secretbox_keybytes
	public :: crypto_secretbox_noncebytes
	public :: crypto_secretbox_macbytes
	public :: crypto_secretbox_keygen
	public :: crypto_secretbox_easy
	public :: crypto_secretbox_open_easy

	interface

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
			import c_int, c_char, c_size_t
			character(kind=c_char), intent(in) :: b1, b2
			integer(kind=c_size_t), value, intent(in) :: siz
			integer(kind=c_int) :: res
		end function s_memcmp

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
