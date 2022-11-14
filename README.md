# sodium
fortran bindings for libsodium

[libsodium c encryption library](https://github.com/jedisct1/libsodium)

[documentation](https://doc.libsodium.org/)

[enc.f90](https://github.com/freevryheid/sodium/blob/main/app/enc.f90) is the fortran version of [ende.c](https://github.com/freevryheid/sodium/blob/main/app/ende.c) to encrypt binary files. 

[ffi](https://github.com/freevryheid/sodium/blob/main/app/ffi.f90) was used to semi-automate the wrapping process, which was possible given the consistency in the c header files.

to use the secure memory functions it is necessary to keep track of the pointer to the string that can later be freed. Given the difference in string definitions in c and fortran, to achieve this the sodium malloc function now returns the fortran string and the pointer to the string. See the new [test](https://github.com/freevryheid/sodium/blob/main/test/tests_crypto_aead_aes256gcm2.f90) that demonstrates this.
