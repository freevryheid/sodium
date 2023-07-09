program tests_version
  use sodium
  implicit none
  if (sodium_version_string().ne."1.0.18") &
    error stop "error: failed sodium_version_string"
  if (sodium_library_version_major().ne.10) &
    error stop "error: failed sodium_library_version_major"
  if (sodium_library_version_minor().ne.3) &
    error stop "error: failed sodium_library_version_minor"
  if (sodium_library_minimal().ne.0) &
    error stop "error: failed sodium_library_minimal"
end program tests_version