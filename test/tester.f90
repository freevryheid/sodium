program tester

	use,intrinsic::iso_fortran_env,only:error_unit
	use testdrive,only:run_testsuite
	use tests_version,only:collect_tests_version
	use tests_crypto_auth,only:collect_tests_crypto_auth
	use tests_utils, only:collect_tests_utils
	use tests_crypto_box,only:collect_tests_crypto_box
	use tests_crypto_secretbox,only:collect_tests_crypto_secretbox
	use tests_crypto_secretstream,only:collect_tests_crypto_secretstream
	use tests_crypto_sign,only:collect_tests_crypto_sign
	use tests_crypto_hash,only:collect_tests_crypto_hash
	use tests_crypto_pwhash,only:collect_tests_crypto_pwhash
	use tests_crypto_aead_aes256gcm,only:collect_tests_crypto_aead_aes256gcm
	use tests_crypto_aead_aes256gcm2,only:collect_tests_crypto_aead_aes256gcm2
	use tests_crypto_scalarmult,only:collect_tests_crypto_scalarmult
	use tests_crypto_generichash,only:collect_tests_crypto_generichash

	implicit none
	integer::i,stat
	character(len=:),allocatable::test

	test="abcdefghijklm"
	! test="m"

	do i=1,len(test)
		stat=0
		selectcase(test(i:i))
			case('a')
				print*,new_line('a'),"a. sodium version tests"
				call run_testsuite(collect_tests_version,error_unit,stat)
			case('b')
				print*,new_line('a'),"b. sodium crypto_auth tests"
				call run_testsuite(collect_tests_crypto_auth,error_unit,stat)
			case('c')
				print*,new_line('a'),"c. sodium utils tests"
				call run_testsuite(collect_tests_utils,error_unit,stat)
			case('d')
				print*,new_line('a'),"d. sodium crypto_box tests"
				call run_testsuite(collect_tests_crypto_box,error_unit,stat)
			case('e')
				print*,new_line('a'),"e. sodium crypto_secretbox tests"
				call run_testsuite(collect_tests_crypto_secretbox,error_unit,stat)
			case('f')
				print*,new_line('a'),"f. sodium crypto_secretstream tests"
				call run_testsuite(collect_tests_crypto_secretstream,error_unit,stat)
			case('g')
				print*,new_line('a'),"g. sodium crypto_sign tests"
				call run_testsuite(collect_tests_crypto_sign,error_unit,stat)
			case('h')
				print*,new_line('a'),"h. sodium crypto_hash tests"
				call run_testsuite(collect_tests_crypto_hash,error_unit,stat)
			case('i')
				print*,new_line('a'),"i. sodium crypto_pwhash tests"
				call run_testsuite(collect_tests_crypto_pwhash,error_unit,stat)
			case('j')
				print*,new_line('a'),"j. sodium crypto_aead_aes256gcm tests"
				call run_testsuite(collect_tests_crypto_aead_aes256gcm,error_unit,stat)
			case('k')
				print*,new_line('a'),"k. sodium crypto_aead_aes256gcmi2 tests"
				call run_testsuite(collect_tests_crypto_aead_aes256gcm2,error_unit,stat)
			case('l')
				print*,new_line('a'),"l. sodium crypto_scalarmult tests"
				call run_testsuite(collect_tests_crypto_scalarmult,error_unit,stat)
			case('m')
				print*,new_line('a'),"m. sodium crypto_generichash tests"
				call run_testsuite(collect_tests_crypto_generichash,error_unit,stat)
			case default
				error stop "test not defined"
		endselect
		if(stat.gt.0)then
			write(error_unit,'(i0,1x,a)')stat,"test(s) failed!"
			error stop
		endif
	enddo

end program tester
