module mod_runtime
  use, intrinsic :: iso_c_binding, only : c_int
  implicit none
  private

  public :: sodium_runtime_has_neon
  public :: sodium_runtime_has_sse2
  public :: sodium_runtime_has_sse3
  public :: sodium_runtime_has_ssse3
  public :: sodium_runtime_has_sse41
  public :: sodium_runtime_has_avx
  public :: sodium_runtime_has_avx2
  public :: sodium_runtime_has_avx512f
  public :: sodium_runtime_has_pclmul
  public :: sodium_runtime_has_aesni
  public :: sodium_runtime_has_rdrand

  interface

    function sodium_runtime_has_neon() &
    bind(c, name='sodium_runtime_has_neon') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_neon

    function sodium_runtime_has_sse2() &
    bind(c, name='sodium_runtime_has_sse2') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_sse2

    function sodium_runtime_has_sse3() &
    bind(c, name='sodium_runtime_has_sse3') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_sse3

    function sodium_runtime_has_ssse3() &
    bind(c, name='sodium_runtime_has_ssse3') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_ssse3

    function sodium_runtime_has_sse41() &
    bind(c, name='sodium_runtime_has_sse41') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_sse41

    function sodium_runtime_has_avx() &
    bind(c, name='sodium_runtime_has_avx') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_avx

    function sodium_runtime_has_avx2() &
    bind(c, name='sodium_runtime_has_avx2') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_avx2

    function sodium_runtime_has_avx512f() &
    bind(c, name='sodium_runtime_has_avx512f') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_avx512f

    function sodium_runtime_has_pclmul() &
    bind(c, name='sodium_runtime_has_pclmul') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_pclmul

    function sodium_runtime_has_aesni() &
    bind(c, name='sodium_runtime_has_aesni') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_aesni

    function sodium_runtime_has_rdrand() &
    bind(c, name='sodium_runtime_has_rdrand') &
    result(res)
      import :: c_int
      integer(kind=c_int) :: res
    end function sodium_runtime_has_rdrand

  end interface

end module mod_runtime
