# sodium
fortran bindings for libsodium

**alpha** version still under development

[libsodium c encryption library](https://github.com/jedisct1/libsodium)

[documentation](https://doc.libsodium.org/)

[enc.f90](https://github.com/freevryheid/sodium/blob/main/app/enc.f90) is the fortran version of [ende.c](https://github.com/freevryheid/sodium/blob/main/app/ende.c) to encrypt binary files. 

[xyz.f90](https://github.com/freevryheid/sodium/blob/main/app/xyz.f90) is a cli app written in fortran, ala [pass](https://www.passwordstore.org/), for encrypted password management using a compressed encrypted file.

[ffi](https://github.com/freevryheid/sodium/blob/main/app/ffi.f90) was used to semi-automate the wrapping process.

the only difference with the c library is that the secure memory "pointer-related" functions have been converted to subroutines to allow allocation of strings outside of fortran - see the tests and apps for examples.
