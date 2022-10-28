module ende

	use,intrinsic::iso_c_binding
	use::sodium
	use::stdlib_error,only:error_stop
	use::stdlib_strings,only:ends_with,replace_all

	implicit none

	private

	type,public::cli
		character(len=:),allocatable::fin
		character(len=:),allocatable::fout
	endtype

	integer,parameter::CHUNK_SIZE=4096

	public::getargs
	public::encrypt
	public::decrypt

	contains

		function getargs()&
		&result(args)
			type(cli)::args
			character(len=:),allocatable::fin,fout
			integer::nargs,arglen,iostat,u,pos
			nargs=command_argument_count()
			if(nargs.ne.1) call error_stop("usage: enc file")
			call get_command_argument(number=1,length=arglen)
			allocate(character(arglen)::fin)
			call get_command_argument(number=1,value=fin,status=iostat)
			if (iostat.ne.0) call error_stop("error: cannot input file path")
			fin=trim(adjustl(fin))
			args%fin=fin
			if(ends_with(fin,".enc"))then
				args%fout=replace_all(fin,".enc",".dec")
			else
				args%fout=fin//".enc"
			endif
		endfunction getargs

		subroutine encrypt(fin,fout,key)
			character(len=:),allocatable::fin,fout,key
			character(len=:),allocatable::bin,bout,header,ad
			integer::in,out,stin,stout,siz,pos,remaining,abytes,hbytes,ret
			integer(kind=c_signed_char)::tag
			integer(kind=c_long_long)::binlen,adlen
			logical::eof
			type(crypto_secretstream_xchacha20poly1305_state)::state
			integer(kind=c_long_long),pointer::blen=>null() ! not cared about chunk lengths
			inquire(file=fin,size=siz)
			abytes=crypto_secretstream_xchacha20poly1305_abytes()
			hbytes=crypto_secretstream_xchacha20poly1305_headerbytes()
			allocate(character(len=hbytes)::header)
			allocate(character(len=CHUNK_SIZE)::bin)
			allocate(character(len=CHUNK_SIZE+abytes)::bout)
			eof=.false.
			ad=c_null_char
			adlen=0
			tag=crypto_secretstream_xchacha20poly1305_tag_message()
			ret=crypto_secretstream_xchacha20poly1305_init_push(state,header,key)
			if(ret.ne.0) call error_stop("error: cannot init push")
			open(newunit=in,file=fin,status="old",access="stream",iostat=stin)
			open(newunit=out,file=fout,status="replace",access="stream",iostat=stout)
			write(out) header
			do while(.not.eof)
				inquire(in,pos=pos)
				remaining=siz-pos+1
				if(remaining.le.CHUNK_SIZE)then
					deallocate(bin)
					deallocate(bout)
					allocate(character(len=remaining)::bin)
					allocate(character(len=remaining+abytes)::bout)
					tag=crypto_secretstream_xchacha20poly1305_tag_final()
					eof=.true.
				endif
				read(in) bin
				binlen=len(bin)
				ret=crypto_secretstream_xchacha20poly1305_push(state,bout,blen,bin,binlen,ad,adlen,tag)
				if(ret.ne.0) call error_stop("error: cannot push")
				write(out) bout
			enddo
			close(out)
			close(in)
		endsubroutine encrypt

		subroutine decrypt(fin,fout,key)
			character(len=:),allocatable::fin,fout,key
			character(len=:),allocatable::bin,bout,header,ad
			integer::in,out,stin,stout,siz,pos,remaining,abytes,hbytes,ret
			integer(kind=c_long_long)::binlen,adlen
			integer(kind=c_signed_char),pointer::tag
			logical::eof
			type(crypto_secretstream_xchacha20poly1305_state)::state
			integer(kind=c_long_long),pointer::blen=>null() ! not cared about chunk lengths
			allocate(tag)
			inquire(file=fin,size=siz)
			abytes=crypto_secretstream_xchacha20poly1305_abytes()
			hbytes=crypto_secretstream_xchacha20poly1305_headerbytes()
			allocate(character(len=hbytes)::header)
			allocate(character(len=CHUNK_SIZE+abytes)::bin)
			allocate(character(len=CHUNK_SIZE)::bout)
			eof=.false.
			ad=c_null_char
			adlen=0
			open(newunit=in,file=fin,status="old",access="stream",iostat=stin)
			open(newunit=out,file=fout,status="replace",access="stream",iostat=stout)
			read(in) header
			ret=crypto_secretstream_xchacha20poly1305_init_pull(state,header,key)
			if(ret.ne.0) call error_stop("error: cannot init pull (incomplete header)")
			do while(.not.eof)
				inquire(in,pos=pos)
				remaining=siz-pos+1
				if(remaining.le.(CHUNK_SIZE+abytes))then
					deallocate(bin)
					deallocate(bout)
					allocate(character(len=remaining)::bin)
					allocate(character(len=remaining-abytes)::bout)
					eof=.true.
				endif
				read(in) bin
				binlen=len(bin)
				ret=crypto_secretstream_xchacha20poly1305_pull(state,bout,blen,tag,bin,binlen,ad,adlen)
				if(ret.ne.0) call error_stop("error: cannot pull (corrupted chunk)")
				write(out) bout
			enddo
			close(out)
			close(in)
			if(tag.ne.crypto_secretstream_xchacha20poly1305_tag_final()) error stop "decryption failed"
		endsubroutine decrypt

endmodule ende


