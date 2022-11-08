# sodium
fortran bindings for libsodium

[libsodium c encryption library](https://github.com/jedisct1/libsodium)

[documentation](https://doc.libsodium.org/)

check apps and tests for application. [enc.f90](https://github.com/freevryheid/sodium/blob/main/app/enc.f90) is the fortran version of [ende.c](https://github.com/freevryheid/sodium/blob/main/app/ende.c) to encrypt binary files. 

the aim was to wrap all functions (including depreciated/obsolete) although I only tested those i needed.

[ffi](https://github.com/freevryheid/sodium/blob/main/app/ffi.f90) was used to semi-automate the wrapping process, which was possible given the consistency in the c header files.
