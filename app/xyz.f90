program epm

	use,intrinsic::iso_c_binding
	use::term
	use::sodium
	use::stdlib_io
	use::mod_epm
	use::ende

	implicit none

	type(account)::a
	type(account),allocatable::aa(:)

	character(len=:),allocatable::non,seed,inp,acc,uid,gen,tmp,res,fin,fout,fzip
	character(len=:),pointer::key
	character(len=256)::input

	integer::i,n,m,cmd_ret,ret,alg,u,v,iotemp,iostat,siz
	logical::exists,found
	integer(kind=c_size_t)::sb,kb,nb,mb,memlimit,hlen,blen,nlen,mlen
	integer(kind=c_long_long)::opslimit
	integer(kind=c_size_t),pointer::plen=>null()

	cmd_ret=cmd()
	if(cmd_ret.eq.0)stop

	! prevent ctrl-c
	call signal(2,1)
	! prevent ctrl-\
	call signal(3,1)

	write(*,'(a)')"xyz version 0.1"
	write(*,'(a)')""

	ret=sodium_init()
	if(ret.eq.-1)errorstop "cannot init sodium"
	sb=crypto_pwhash_saltbytes()
	kb=crypto_box_seedbytes()
	nb=crypto_secretbox_noncebytes()
	mb=crypto_secretbox_macbytes()
	opslimit=crypto_pwhash_opslimit_sensitive()
	memlimit=crypto_pwhash_memlimit_sensitive()
	alg=crypto_pwhash_alg_default()
	allocate(character(len=sb)::seed)
	allocate(character(len=sb)::gen)
	n=0
	fin="xy.t"
	fout="xy.p"
	fzip="xy.z"

	do
		write(*,'(a)')"type and enter your key"
		write(*,'(a)')"note that input is hidden and it will"
		write(*,'(a)')"take a while to process after entered"
		! write(*,'(a)')"if you hit CTRL-C now to exit then run 'reset'"
		! write(*,'(a)')"to flush/reset the hidden terminal"
		call c_setmode(1)
		write(*,'(a)',advance='no')"key? "
		read(*,'(a)')input
		call c_setmode(0)
		if(len(trim(input)).gt.0)then
			exit
		else
			write(*,'(a,a)')new_line('a'),"your key cannot be blank"
		endif
	enddo

	call sodium_malloc(key,kb)
	ret=crypto_pwhash(key,kb,input,len(input,kind=c_size_t),seed,opslimit,memlimit,alg)

	inquire(file=fzip,exist=exists)
	if(.not.exists)then
		call randombytes_buf(seed,sb)
		open(newunit=u,file=fin,status="new",access="stream",iostat=iostat)
		write(u)seed
		write(u)n
		close(u)
		call encrypt(fin,fout,key)
		open(newunit=u,file=fin,status="old",iostat=iostat)
		if(iostat.eq.0)close(u,status="delete")
		ret=zip_deflate_file(fout,fzip)
		open(newunit=u,file=fout,status="old",iostat=iostat)
		if(iostat.eq.0)close(u,status="delete")
	endif

	a=account(siz1=0,siz2=0,siz3=0)
	aa=[a]
	ret=zip_inflate_file(fzip,fout)
	open(newunit=u,file=fzip,status="old",iostat=iostat)
	if(iostat.eq.0)close(u,status="delete")
	call decrypt(fout,fin,key)
	open(newunit=u,file=fout,status="old",iostat=iostat)
	if(iostat.eq.0)close(u,status="delete")
	open(newunit=u,file=fin,status="old",access="stream",iostat=iostat)
	read(u)seed
	read(u)n
	if(n.gt.0)then
		do i=1,n
			a=account(siz1=0,siz2=0,siz3=0)
			allocate(character(len=nb)::a%non1)
			read(u)a%non1
			read(u)a%siz1
			allocate(character(len=a%siz1)::a%inp1)
			read(u)a%inp1
			allocate(character(len=nb)::a%non2)
			read(u)a%non2
			read(u)a%siz2
			allocate(character(len=a%siz2)::a%inp2)
			read(u)a%inp2
			allocate(character(len=nb)::a%non3)
			read(u)a%non3
			read(u)a%siz3
			allocate(character(len=a%siz3)::a%inp3)
			read(u)a%inp3
			aa=[aa,a]
		enddo
	endif
	close(u)

	selectcase(cmd_ret)
		case(1)
			write(*,'(a)') new_line('a')//"adding new account(s) ..."
			outer: do
				a=account(siz1=0,siz2=0,siz3=0)
				allocate(character(len=nb)::a%non1)
				allocate(character(len=nb)::a%non2)
				allocate(character(len=nb)::a%non3)
				call randombytes_buf(a%non1,nb)
				call randombytes_buf(a%non2,nb)
				call randombytes_buf(a%non3,nb)
				do
					write(*,'(a)') "enter blank to exit"
					write(*,'(a)',advance='no') "acc? "
					read(*,'(a)') input
					inp=trim(input)
					siz=len(inp)
					acc=inp
					if(siz.eq.0)exit outer
					found=.false.
					do i=2,size(aa)
						tmp=dec(aa(i)%inp1,mb,aa(i)%non1,key)
						if(inp.eq.tmp)then
							found=.true.
							exit
						endif
					enddo
					if(found)then
						write(*,'(a)')"account "//acc//" already exists"
						exit outer
					else
						a%inp1=enc(inp,mb,a%non1,key)
						a%siz1=siz+mb
						exit
					endif
				enddo
				write(*,'(a)') "uid can be blank"
				write(*,'(a)',advance='no') "uid? "
				read(*,'(a)') input
				inp=trim(input)
				siz=len(inp)
				a%inp2=enc(inp,mb,a%non2,key)
				a%siz2=siz+mb
				siz=0
				do while(siz.eq.0)
					write(*,'(a)') "pwd cannot be blank"
					write(*,'(a)') "enter blank to autogen"
					write(*,'(a)',advance='no') "pwd? "
					read(*,'(a)') input
					inp=trim(input)
					siz=len(inp)
					if(siz.eq.0)then
						call execute_command_line("pwgen -scnyN1 16 > delme",exitstat=iotemp)
						if(iotemp.eq.0)then
							open(file="delme",newunit=v,iostat=iotemp)
							if(iotemp.eq.0)call getline(v,gen,iotemp)
							close(v,status="delete",iostat=iotemp)
							siz=len(gen)
							write(*,'(a)')"gen: "//gen
							a%inp3=enc(gen,mb,a%non3,key)
							a%siz3=sb+mb
						else
							write(*,'(a)')"sorry, unable to autogen pwd"
						endif
					else
						a%inp3=enc(inp,mb,a%non3,key)
						a%siz3=siz+mb
					endif
				enddo
				aa=[aa,a]
				open(newunit=u,file=fin,status="replace",access="stream",iostat=iostat)
				write(u)seed
				write(u)size(aa)-1
				do i=2,size(aa)
					write(u)aa(i)%non1
					write(u)aa(i)%siz1
					write(u)aa(i)%inp1
					write(u)aa(i)%non2
					write(u)aa(i)%siz2
					write(u)aa(i)%inp2
					write(u)aa(i)%non3
					write(u)aa(i)%siz3
					write(u)aa(i)%inp3
				enddo
				close(u)
				write(*,'(a,a,a)')"account ",acc," added"
			enddo outer
			write(*,'(a)')"... done."

		case(2)
			write(*,'(a)')new_line('a')//"listing accounts ..."
			if(size(aa).eq.1)then
				write(*,'(a)')"no accounts found"
			else
				do i=2,size(aa)
					write(*,'(a)')dec(aa(i)%inp1,mb,aa(i)%non1,key)
				enddo
			endif
			write(*,'(a)')"... done."

		case(3)
			write(*,'(a)') new_line('a')//"changing account(s) info ..."
			do
				write(*,'(a)')"enter blank to exit"
				write(*,'(a)',advance='no')"acc? "
				read(*,'(a)')input
				inp=trim(input)
				siz=len(inp)
				acc=inp
				if(siz.eq.0)then
					write(*,'(a)')"... done."
					exit
				else
					found=.false.
					do i=2,size(aa)
						tmp=dec(aa(i)%inp1,mb,aa(i)%non1,key)
						if(inp.eq.tmp)then
							write(*,'(a)')"enter to keep existing uid"
							write(*,'(a)')"shown in parenthesis"
							uid=dec(aa(i)%inp2,mb,aa(i)%non2,key)
							if(len(uid).gt.0)write(*,'(a)')"enter 'clr' to clear it"
							write(*,'(a,a,a)',advance='no')"uid (",uid,")? "
							read(*,'(a)')input
							inp=trim(input)
							siz=len(inp)
							if(siz.gt.0)then
								if(inp.eq."clr")then
									inp=""
									siz=0
								endif
								deallocate(aa(i)%non2)
								allocate(character(len=nb)::aa(i)%non2)
								call randombytes_buf(aa(i)%non2,nb)
								deallocate(aa(i)%inp2)
								allocate(character(len=siz+mb)::aa(i)%inp2)
								aa(i)%inp2=enc(inp,mb,aa(i)%non2,key)
								aa(i)%siz2=siz+mb
							endif
							write(*,'(a)')"enter to keep existing pwd"
							write(*,'(a)')"shown in parenthesis"
							write(*,'(a)')"enter 'gen' to autogen a new pwd"
							write(*,'(a,a,a)',advance='no')"pwd (",dec(aa(i)%inp3,mb,aa(i)%non3,key),")? "
							read(*,'(a)')input
							inp=trim(input)
							siz=len(inp)
							if(siz.gt.0)then
								if(inp.eq."gen")then
									call execute_command_line("pwgen -scnyN1 16 > delme",exitstat=iotemp)
									if(iotemp.eq.0)then
										open(file="delme",newunit=v,iostat=iotemp)
										if(iotemp.eq.0)call getline(v,gen,iotemp)
										close(v,status="delete",iostat=iotemp)
										write(*,'(a)')"gen: "//gen
										deallocate(aa(i)%non3)
										allocate(character(len=nb)::aa(i)%non3)
										call randombytes_buf(aa(i)%non3,nb)
										deallocate(aa(i)%inp3)
										allocate(character(len=sb+mb)::aa(i)%inp3)
										aa(i)%inp3=enc(gen,mb,aa(i)%non3,key)
										aa(i)%siz3=sb+mb
									else
										write(*,'(a)')"sorry, unable to autogen pwd"
										write(*,'(a)')"falling back to the existing one"
									endif
								else
									deallocate(aa(i)%non3)
									allocate(character(len=nb)::aa(i)%non3)
									call randombytes_buf(aa(i)%non3,nb)
									deallocate(aa(i)%inp3)
									allocate(character(len=sb+mb)::aa(i)%inp3)
									aa(i)%inp3=enc(inp,mb,aa(i)%non3,key)
									aa(i)%siz3=siz+mb
								endif
							endif
							found=.true.
							exit
						endif
					enddo
					if(.not.found)then
						write(*,'(a)')"account "//inp//" not found"
					else
						open(newunit=u,file=fin,status="replace",access="stream",iostat=iostat)
						write(u)seed
						write(u)size(aa)-1
						do i=2,size(aa)
							write(u)aa(i)%non1
							write(u)aa(i)%siz1
							write(u)aa(i)%inp1
							write(u)aa(i)%non2
							write(u)aa(i)%siz2
							write(u)aa(i)%inp2
							write(u)aa(i)%non3
							write(u)aa(i)%siz3
							write(u)aa(i)%inp3
						enddo
						close(u)
						write(*,'(a,a,a)')"account ",acc," updated"
					endif
				endif
			enddo

		case(4)
			write(*,'(a)') new_line('a')//"showing account(s) info ..."
			do
				write(*,'(a)')"enter blank to exit"
				write(*,'(a)',advance='no')"acc? "
				read(*,'(a)')input
				inp=trim(input)
				siz=len(inp)
				if(siz.eq.0)then
					write(*,'(a)')"... done."
					exit
				else
					found=.false.
					do i=2,size(aa)
						tmp=dec(aa(i)%inp1,mb,aa(i)%non1,key)
						if(inp.eq.tmp)then
							write(*,'(a)')"acc: "//dec(aa(i)%inp1,mb,aa(i)%non1,key)
							write(*,'(a)')"uid: "//dec(aa(i)%inp2,mb,aa(i)%non2,key)
							write(*,'(a)')"pwd: "//dec(aa(i)%inp3,mb,aa(i)%non3,key)
							found=.true.
							exit
						endif
					enddo
					if(.not.found)write(*,'(a)')"account "//inp//" not found"
				endif
			enddo

		case(5)
			write(*,'(a)') new_line('a')//"deleting account(s) ..."
			do
				write(*,'(a)')"enter blank to exit"
				write(*,'(a)',advance='no')"acc? "
				read(*,'(a)')input
				inp=trim(input)
				siz=len(inp)
				acc=inp
				if(siz.eq.0)then
					write(*,'(a)')"... done."
					exit
				else
					found=.false.
					do i=2,size(aa)
						tmp=dec(aa(i)%inp1,mb,aa(i)%non1,key)
						if(inp.eq.tmp)then
							n=i
							found=.true.
							exit
						endif
					enddo
					if(.not.found)then
						write(*,'(a)')"account "//inp//" not found"
					else
						aa=[aa(:n-1),aa(n+1:)]
						open(newunit=u,file=fin,status="replace",access="stream",iostat=iostat)
						write(u)seed
						write(u)size(aa)-1
						do i=2,size(aa)
							write(u)aa(i)%non1
							write(u)aa(i)%siz1
							write(u)aa(i)%inp1
							write(u)aa(i)%non2
							write(u)aa(i)%siz2
							write(u)aa(i)%inp2
							write(u)aa(i)%non3
							write(u)aa(i)%siz3
							write(u)aa(i)%inp3
						enddo
						close(u)
						write(*,'(a,a,a)')"account ",acc," deleted"
					endif
				endif
			enddo

		case default

	endselect

	call encrypt(fin,fout,key)
	call sodium_free(key)

	open(newunit=u,file=fin,status="old",iostat=iostat)
	if(iostat.eq.0)close(u,status="delete")
	ret=zip_deflate_file(fout,fzip)
	open(newunit=u,file=fout,status="old",iostat=iostat)
	if(iostat.eq.0)close(u,status="delete")

endprogram epm
