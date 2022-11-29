program enc

	use,intrinsic::iso_c_binding
	use::stdlib_error,only:error_stop
	use::stdlib_strings,only:ends_with
	use::sodium
	use::ende

	implicit none

	type(cli)::args
	character(len=:),pointer::key
	integer::u,iostat,ret
	integer(kind=c_size_t)::kbytes
	logical::keyexists

	ret=sodium_init()
	if(ret.ne.0) call error_stop("error: cannot init sodium")
	kbytes=crypto_secretstream_xchacha20poly1305_keybytes()
	call sodium_malloc(key,kbytes)
	args=getargs()
	if(ends_with(args%fin,".enc"))then
		inquire(file="key",exist=keyexists)
		if(keyexists)then
			open(newunit=u,file="key",status="old",access="stream",iostat=iostat)
			read(u) key
			close(u)
			ret=decrypt(args%fin,args%fout,key)
		else
			call error_stop("error: cannot decrypt file without a key")
		endif
	else
		call crypto_secretstream_xchacha20poly1305_keygen(key)
		open(newunit=u,file="key",status="replace",access="stream",iostat=iostat)
		write(u) key
		close(u)
		ret=encrypt(args%fin,args%fout,key)
	endif

	call sodium_free(key)

endprogram enc
