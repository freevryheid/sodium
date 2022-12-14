module tests_crypto_secretstream

	use, intrinsic :: iso_c_binding
	use sodium
	use testdrive

	implicit none

	private

	public :: collect_tests_crypto_secretstream

	contains

		subroutine collect_tests_crypto_secretstream(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[new_unittest("test for secret_stream",test_ss)]
		endsubroutine collect_tests_crypto_secretstream

		subroutine test_ss(error)
			type(error_type),allocatable,intent(out)::error
			type(crypto_secretstream_xchacha20poly1305_state)::state
			character(len=:),allocatable::key,header,m1,m2,m3,c1,c2,c3,r1,r2,r3,ad
			! integer(kind=c_signed_char),pointer::tag
			character(len=1)::tag

			! integer(kind=c_signed_char)::tag0
			character(len=1)::tag0
			integer(kind=c_size_t)::kb,hb,ab
			integer::res
			integer(kind=c_long_long),pointer::c1l,c2l,c3l
			integer(kind=c_long_long)::m1l,m2l,m3l
			integer(kind=c_long_long)::r1l,r2l,r3l,adlen

			! test tags
			! allocate(tag)
			! allocate(character(len=1)::tag)
			allocate(c1l)
			allocate(c2l)
			allocate(c3l)

			call check(error,int(crypto_secretstream_xchacha20poly1305_tag_message()),0)
			call check(error,int(crypto_secretstream_xchacha20poly1305_tag_push()),1)
			call check(error,int(crypto_secretstream_xchacha20poly1305_tag_rekey()),2)
			call check(error,int(crypto_secretstream_xchacha20poly1305_tag_final()),3)

			m1="Arbitrary data to encrypt"
			m2="split into"
			m3="three messages"
			m1l=len(m1)
			m2l=len(m2)
			m3l=len(m3)
			ab=crypto_secretstream_xchacha20poly1305_abytes()
			kb=crypto_secretstream_xchacha20poly1305_keybytes()
			hb=crypto_secretstream_xchacha20poly1305_headerbytes()
			r1l=m1l+ab
			r2l=m2l+ab
			r3l=m3l+ab
			ad=c_null_char
			adlen=0
			! tag0=crypto_secretstream_xchacha20poly1305_tag_message()
			tag0=char(crypto_secretstream_xchacha20poly1305_tag_message())

			allocate(character(len=kb)::key)
			allocate(character(len=hb)::header)
			allocate(character(len=r1l)::c1)
			allocate(character(len=r2l)::c2)
			allocate(character(len=r3l)::c3)
			allocate(character(len=m1l)::r1)
			allocate(character(len=m2l)::r2)
			allocate(character(len=m3l)::r3)

			! assign pointers
			c1l=r1l
			c2l=r2l
			c3l=r3l

			! shared secret key required to encrypt/decrypt the stream
			call crypto_secretstream_xchacha20poly1305_keygen(key)


			! encrypt
			! set up a new stream: initialize the state and create the header
			res=crypto_secretstream_xchacha20poly1305_init_push(state,header,key)
			call check(error,res,0,"init_push")

			! encrypt the first chunk.
			! c1 will contain an encrypted, authenticated representation of m1
			res=crypto_secretstream_xchacha20poly1305_push(state,c1,c1l,c_str(m1),m1l,ad,adlen,tag0)
			call check(error,res,0,"push1")

			! encrypt the second chunk.
			! c2 will contain an encrypted, authenticated representation of m2
			res=crypto_secretstream_xchacha20poly1305_push(state,c2,c2l,c_str(m2),m2l,ad,adlen,tag0)
			call check(error,res,0,"push2")

			! encrypt the last chunk.
			! c3 will contain an encrypted, authenticated representation of m3
			! note the `TAG_FINAL` tag to indicate that this is the final chunk.
			! tag0=crypto_secretstream_xchacha20poly1305_tag_final()
			tag0=char(crypto_secretstream_xchacha20poly1305_tag_final())
			res=crypto_secretstream_xchacha20poly1305_push(state,c3,c3l,c_str(m3),m3l,ad,adlen,tag0)
			call check(error,res,0,"push3")


			! decrypt
			! decrypt the stream: initializes the state, using the key and a header
			res=crypto_secretstream_xchacha20poly1305_init_pull(state,header,key)
			call check(error,res,0,"init_pull")

			! decrypt the first chunk. A real application would probably use
			! a loop, that reads data from the network or from disk, and exits after
			! an error, or after the last chunk (with a TAG_FINAL tag) has been
			! decrypted.
			res=crypto_secretstream_xchacha20poly1305_pull(state,r1,c1l,tag,c1,r1l,ad,adlen)
			call check(error,res,0,"pull1")
			! tag0=crypto_secretstream_xchacha20poly1305_tag_message()
			tag0=char(crypto_secretstream_xchacha20poly1305_tag_message())
			call check(error,tag,tag0,"tag_message1")
			call check(error,r1,m1,"r1=m1")

			! decrypt the second chunk, store the result into r2
			res=crypto_secretstream_xchacha20poly1305_pull(state,r2,c2l,tag,c2,r2l,ad,adlen)
			call check(error,res,0,"pull2")
			call check(error,tag,tag0,"tag_message2")
			call check(error,r2,m2,"r2=m2")

			! decrypt the last chunk, store the result into r3
			res=crypto_secretstream_xchacha20poly1305_pull(state,r3,c3l,tag,c3,r3l,ad,adlen)
			call check(error,res,0,"pull3")
			! tag0=crypto_secretstream_xchacha20poly1305_tag_final()
			tag0=char(crypto_secretstream_xchacha20poly1305_tag_final())
			call check(error,tag,tag0,"tag_final")
			call check(error,r3,m3,"r3=m3")
			! the tag indicates that this is the final chunk, no need to read and decrypt more

			if (allocated(error)) return
		endsubroutine test_ss

endmodule tests_crypto_secretstream
