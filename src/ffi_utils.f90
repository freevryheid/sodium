module ffi_utils

	use stdlib_error
	use stdlib_io
	use stdlib_string_type
	use stdlib_strings
	use scanner

	implicit none
	private

	integer, parameter,public::UNK=0
	integer, parameter,public::FUN=1
	integer, parameter,public::SUB=2
	integer, parameter,public::DEF=3

	type,public::param
		type(string_type)::nam
		type(string_type)::typ
		type(string_type)::imp
		type(string_type)::arr
		logical::ptr=.false.
	endtype

	type,public::proc
		type(string_type)::cod
		type(param),dimension(:),allocatable::prms ! parameters
		integer::prc=UNK ! procedure UNK,FNC, SUB or DEF
		type(string_type)::typ
		type(string_type)::nam
		logical::ptr=.false.
	endtype

	public::get_file
	public::params2string

	contains

		subroutine param_defs(procs)
			type(proc),dimension(:),allocatable,intent(inout)::procs
			type(param)::aparam
			type(param),dimension(:),allocatable::params
			type(string_type)::st,nam,typ,arr,st0
			integer::i,pos1,pos2,n,m
			type(textscanner)::txt
			do i=1,size(procs)
				st=string_type()
				aparam=param(nam=st,typ=st,imp=st,arr=st)
				params=[aparam]
				st=procs(i)%cod
				! print*,st
				if(.not.procs(i)%prc.eq.DEF)then
					pos1=find(st,"(")
					pos2=find(st,")")
					st=char(st,pos1,pos2)
					call txt%new(st)
					call txt%last()
					pos2=txt%pos
					do
						st0=string_type()
						aparam=param(nam=st0,typ=st0,imp=st0,arr=st0)
						call txt%jump_from(32)
						pos1=txt%pos
						nam=char(st,pos1+1,pos2-1)
						nam=trim(adjustl(nam))
						n=find(nam,"*") ! pointers
						if(n.gt.0)then
							aparam%ptr=.true.
							nam=replace_all(nam,"*","")
						endif
						n=find(nam,"[") ! arrays
						if(n.gt.0)then
							m=find(nam,"]")
							arr=char(nam,n+1,m-1)
							aparam%arr=arr
							nam=char(nam,1,n-1)
						endif
						aparam%nam=nam
						pos2=pos1
						call txt%jump_from(ichar(","))
						pos1=txt%pos
						typ=char(st,pos1+1,pos2-1)
						typ=trim(adjustl(typ))
						aparam%typ=typ
						params=[params,aparam]
						pos2=pos1
						if(pos1.eq.1) exit
					enddo
					params=[params(2:)]
					procs(i)%prms=params(ubound(params,1):1:-1) ! reverse it
				endif
			enddo
		endsubroutine param_defs

		subroutine proc_defs(procs)
			type(proc),dimension(:),allocatable,intent(inout)::procs
			type(string_type)::st,nam,typ
			type(textscanner)::txt
			integer::i,pos1,pos2,n,prc
			do i=1,size(procs)
				prc=UNK
				st=procs(i)%cod
				if(procs(i)%prc.eq.DEF) cycle
				call txt%new(st)
				call txt%last()
				call txt%jump_from(ichar("("))
				pos2=txt%pos
				call txt%jump_from(32)
				pos1=txt%pos
				nam=char(st,pos1+1,pos2-1)
				n=find(nam,"*")
				if(n.gt.0)then
					procs(i)%ptr=.true.
					prc=FUN
					nam=replace_all(nam,"*","")
				endif
				procs(i)%nam=nam
				typ=char(st,1,pos1-1)
				procs(i)%typ=typ
				if(typ.eq."void".and.prc.ne.FUN)then
					prc=SUB
				else
					prc=FUN
				endif
				procs(i)%prc=prc
			enddo
		endsubroutine proc_defs

		function get_file()result(procs)
			! returns an array of procs comprising the procedures in the input file
			character(len=:),allocatable::fin,prog
			type(proc),dimension(:),allocatable::procs
			type(param),dimension(:),allocatable::params
			type(proc)::aproc
			type(param)::aparam
			type(string_type)::st,sst
			integer::nargs,arglen,iostat,u,pos
			logical::export=.false.
			logical::done=.false.
			call get_command_argument(number=0,length=arglen)
			allocate(character(arglen)::prog)
			call get_command_argument(number=0,value=prog,status=iostat)
			nargs=command_argument_count()
			if(nargs.ne.1) call error_stop("usage: "//prog//" path_to_c_header")
			call get_command_argument(number=1,length=arglen)
			allocate(character(arglen)::fin)
			call get_command_argument(number=1,value=fin,status=iostat)
			if (iostat.ne.0) call error_stop("error: cannot retrieve path")
			u=open(fin,iostat=iostat)
			if(iostat.ne.0) call error_stop("error: cannot open "//fin)
			st=string_type()
			aparam=param(nam=st,typ=st,imp=st,arr=st)
			params = [aparam]
			aproc=proc(cod=st,prms=params,nam=st,typ=st)
			procs = [aproc]
			do while(iostat.eq.0)
				st=string_type()
				aproc=proc(cod=st,prms=params,nam=st,typ=st)
				call getline(u,st,iostat)
				st=trim(adjustl(reduce(st)))
				if(export)then
					if(ends_with(st,";"))then
						done=.true.
						export=.false.
					endif
					pos=find(st,")")
					if(pos.gt.0) st=char(st,1,pos)
					if(.not.starts_with(st,"__")) sst=sst//st
				endif
				if(done)then
					aproc%cod=sst
					procs=[procs,aproc]
					done=.false.
				endif
				if(starts_with(st,"SODIUM_EXPORT"))then
					sst=string_type()
					export=.true.
				endif
				if((starts_with(st,"#define")).and.(.not.ends_with(st,"\")))then
						aproc%cod=st
						aproc%prc=DEF
						procs=[procs,aproc]
				endif
			enddo
			close(u)
			procs=[procs(2:)]
			call proc_defs(procs)
			call param_defs(procs)
		endfunction get_file

		function reduce(string) result(res)
			! removes double spaces and spaces around commas.
			type(string_type),intent(in)::string
			type(string_type)::res
			res=string
			do while(find(res,"  ").gt.0)
				res=replace_all(res,"  "," ")
			enddo
			do while(find(res,", ").gt.0)
				res=replace_all(res,", ",",")
			enddo
			do while(find(res," ,").gt.0)
				res=replace_all(res," ,",",")
			enddo
		endfunction reduce

		function params2string(prms)result(res)
			type(param),allocatable,dimension(:)::prms
			type(string_type)::res,comma
			integer::i,siz
			siz=size(prms)
			res=string_type()
			comma=string_type(",")
			if(siz.gt.1)then
				do i=1,siz
					res=res//prms(i)%nam
					if(i.lt.siz) res=res//comma
				enddo
			else
				res=prms(1)%nam
			endif
		endfunction params2string

endmodule ffi_utils

