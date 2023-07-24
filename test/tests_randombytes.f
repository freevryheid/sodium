program tests_randombytes
  use :: sodium
  implicit none

  block
    if (randombytes_seedbytes().ne.PARAM_randombytes_SEEDBYTES) &
      error stop "error: randombytes_seedbytes failed"
  end block

end program tests_randombytes