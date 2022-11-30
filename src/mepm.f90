module mod_epm

	use,intrinsic::iso_c_binding
	use::sodium
	use::zlib

	implicit none

	private

	type,public::account
		character(len=:),allocatable::non1
		integer::siz1
		character(len=:),allocatable::inp1
		character(len=:),allocatable::non2
		integer::siz2
		character(len=:),allocatable::inp2
		character(len=:),allocatable::non3
		integer::siz3
		character(len=:),allocatable::inp3
	endtype

	public::cmd,enc,dec,zip_inflate_file,zip_deflate_file,del_if_exists
	integer, parameter :: CHUNK = 16384

	contains

		function cmd()result(ret)
			integer::nargs,ret
			character(len=3)::c3
			nargs=command_argument_count()
			if(nargs.ne.1)then
				c3="err"
			else
				call get_command_argument(number=1,value=c3)
			endif
			selectcase(c3)
				case("err")
					write(*,'(a)')"command error"
					write(*,'(a)')"run 'xyz hlp' for help"
					ret=0
				case("hlp")
					write(*,'(a)')"xyz version 0.1"
					write(*,'(a)')"simple encrypted password manager"
					write(*,'(a)')"a cli to manage encrypted passwords for user accounts"
					write(*,'(a)')""
					write(*,'(a)')"usage: xyz cmd"
					write(*,'(a)')""
					write(*,'(a)')"where cmd is one of following:"
					write(*,'(a)')""
					write(*,'(a)')"hlp - display this help message"
					write(*,'(a)')"add - add new account(s)"
					write(*,'(a)')"lst - list account(s)"
					write(*,'(a)')"chg - change account(s) info"
					write(*,'(a)')"shw - show account(s) info"
					write(*,'(a)')"del - delete account(s)"
					write(*,'(a)')""
					write(*,'(a)')"you will be prompted for a key (password)"
					write(*,'(a)')"that is used to encrpyt/decrypt the data you enter"
					write(*,'(a)')"and the only password you need to remember."
					write(*,'(a)')"use the same key for all commands."
					write(*,'(a)')""
					write(*,'(a)')"for security reasons your key is never saved"
					write(*,'(a)')"or validated and cannot be recovered if lost."
					write(*,'(a)')"run 'pwd lst' or 'pwd shw' to verify that"
					write(*,'(a)')"your key returns valid info."
					write(*,'(a)')""
					write(*,'(a)')"account info is stored in xy.z, a compressed"
					write(*,'(a)')"encrypted binary file with encrypted account"
					write(*,'(a)')"info that can only be decrypted using your key."
					write(*,'(a)')"keep a backup of your xy.z file: 'cp xy.z xy.b'"
					write(*,'(a)')""
					write(*,'(a)')"xyz optionally uses pwgen to generate passwords."
					write(*,'(a)')"run 'pwgen' to check if it is installed."
					ret=0
				case("add")
					ret=1
				case("lst")
					ret=2
				case("chg")
					ret=3
				case("shw")
					ret=4
				case("del")
					ret=5
				case default
					write(*,'(a)') "unkwown command: "//c3
					write(*,'(a)') "run 'xyz hlp' for help"
					ret=0
			endselect
		endfunction cmd

		function enc(in,mb,non,key)result(out)
			character(len=:),allocatable::in,out,non
			character(len=:),pointer::key
			integer(kind=c_size_t)::mb,siz
			integer::ret
			siz=len(in)
			allocate(character(len=siz+mb)::out)
			ret=crypto_secretbox_easy(out,in,siz,non,key)
		endfunction enc

		function dec(in,mb,non,key)result(out)
			character(len=:),allocatable::in,out,non
			character(len=:),pointer::key
			integer(kind=c_size_t)::mb,siz
			integer::ret
			siz=len(in)
			allocate(character(len=siz-mb)::out)
			ret=crypto_secretbox_open_easy(out,in,siz,non,key)
		endfunction dec

		! from zlib test
		integer function zip_deflate_file(source, dest) result(rc)
			character(len=*), intent(in) :: source
			character(len=*), intent(in) :: dest
			! integer,          intent(in) :: level
			integer :: level
			character(len=CHUNK), target :: in
			character(len=CHUNK), target :: out
			character      :: byte
			integer        :: err, flush, have
			integer        :: i, n
			integer        :: in_unit, out_unit
			type(z_stream) :: strm
			level = Z_DEFAULT_COMPRESSION
			rc = deflate_init(strm, level)
			if (rc /= Z_OK) return
			def_block: block
				rc = Z_ERRNO
				open (access='stream', action='read', file=source, form='unformatted', iostat=err, newunit=in_unit, status='old')
				if (err /= 0) exit def_block
				open (access='stream', action='write', file=dest, form='unformatted', iostat=err, newunit=out_unit, status='replace')
				if (err /= 0) exit def_block
				do
					n = 0
					flush = Z_NO_FLUSH
					do i = 1, CHUNK
						read (in_unit, iostat=err) byte
						if (is_iostat_end(err)) then
							flush = Z_FINISH
							exit
						end if
						in(i:i) = byte
						n = n + 1
					end do
					strm%avail_in = n
					strm%next_in = c_loc(in)
					do
						strm%avail_out = CHUNK
						strm%next_out = c_loc(out)
						rc = deflate(strm, flush)
						if (rc == Z_STREAM_ERROR) exit def_block
						have = CHUNK - strm%avail_out
						write (out_unit, iostat=err) out(1:have)
						if (err /= 0) exit def_block
						if (strm%avail_out /= 0) exit
					end do
					if (strm%avail_in /= 0) exit def_block
					if (flush == Z_FINISH) exit
				end do
				if (rc /= Z_STREAM_END) exit def_block
				rc = Z_OK
			end block def_block
			err = deflate_end(strm)
			close (out_unit)
			close (in_unit)
		end function zip_deflate_file

		integer function zip_inflate_file(source, dest) result(rc)
			character(len=*), intent(in) :: source
			character(len=*), intent(in) :: dest
			character(len=CHUNK), target :: in
			character(len=CHUNK), target :: out
			character      :: byte
			integer        :: err, have
			integer        :: i, n
			integer        :: in_unit, out_unit
			type(z_stream) :: strm
			rc = inflate_init(strm)
			if (rc /= Z_OK) return
			inf_block: block
				rc = Z_ERRNO
				open (access='stream', action='read', file=source, form='unformatted', iostat=err, newunit=in_unit, status='old')
				if (err /= 0) exit inf_block
				open (access='stream', action='write', file=dest, form='unformatted', iostat=err, newunit=out_unit, status='replace') ! replace/scratch
				if (err /= 0) exit inf_block
				do
					n = 0
					do i = 1, CHUNK
						read (in_unit, iostat=err) byte
						if (is_iostat_end(err)) exit
						in(i:i) = byte
						n = n + 1
					end do
					strm%avail_in = n
					strm%next_in = c_loc(in)
					do
						strm%avail_out = CHUNK
						strm%next_out = c_loc(out)
						rc = inflate(strm, Z_NO_FLUSH)
						if (rc == Z_STREAM_ERROR) exit inf_block
						if (rc == Z_NEED_DICT) exit inf_block
						if (rc == Z_DATA_ERROR) exit inf_block
						if (rc == Z_MEM_ERROR) exit inf_block
						have = CHUNK - strm%avail_out
						write (out_unit, iostat=err) out(1:have)
						if (err /= 0) exit inf_block
						if (strm%avail_out /= 0) exit
					end do
					if (rc == Z_STREAM_END) exit
				end do
				rc = Z_OK
			end block inf_block
			err = inflate_end(strm)
			close (out_unit)
			close (in_unit)
		end function zip_inflate_file

		subroutine del_if_exists(f)
			character(len=*)::f
			integer::u,iostat
			logical::exists
			inquire(file=f,exist=exists)
			if(exists)then
				open(newunit=u,file=f,status="old",access="stream",iostat=iostat)
				if(iostat.eq.0)close(u,status="delete")
			endif
		endsubroutine del_if_exists

endmodule mod_epm
