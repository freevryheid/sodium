module mod_epm

	use,intrinsic::iso_c_binding
	use::sodium

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

	public::cmd,enc,dec

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
					write(*,'(a)')"run 'epm hlp' for help"
					ret=0
				case("hlp")
					write(*,'(a)')"epm version 0.1"
					write(*,'(a)')"encrypted password manager"
					write(*,'(a)')"a cli to manage encrypted passwords for user accounts"
					write(*,'(a)')""
					write(*,'(a)')"usage: epm cmd"
					write(*,'(a)')""
					write(*,'(a)')"where cmd is one of following:"
					write(*,'(a)') ""
					write(*,'(a)') "hlp - display this help message"
					write(*,'(a)') "add - add new account(s)"
					write(*,'(a)') "lst - list account(s)"
					write(*,'(a)') "chg - change account(s) info"
					write(*,'(a)') "shw - show account(s) info"
					write(*,'(a)') "del - delete account(s)"
					write(*,'(a)')""
					write(*,'(a)') "you will be prompted for a key (password)"
					write(*,'(a)') "that is used to encrpyt/decrypt the data you enter"
					write(*,'(a)') "and the only password you need to remember"
					write(*,'(a)') "use the same key for all commands"
					write(*,'(a)') "for security reasons your key is never saved"
					write(*,'(a)') "or validated - you've been warned"
					write(*,'(a)')""
					write(*,'(a)') "account info is stored in savemy.pwd,"
					write(*,'(a)') "an encrypted binary file with encrypted account"
					write(*,'(a)') "info that can only be decrypted using your key"
					write(*,'(a)')""
					write(*,'(a)') "pwd optionally uses pwgen to generate passwords"
					write(*,'(a)') "run 'pwgen' now to check if it is installed"
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
					write(*,'(a)') "run 'pwd hlp' for help"
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

endmodule mod_epm
