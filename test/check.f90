program unittest
use iso_fortran_env, only: stderr => error_unit 
use testing
implicit none

type(Test) :: tests(1)
integer :: i
integer :: n_failed_tests

n_failed_tests = 0
call test_flora(tests(1)) 

do i = 1,size(tests)
  if ( tests(i) % failed() ) then
    write(stderr,*) "Test", i, " failed: ", tests(i) % msg()
    n_failed_tests = n_failed_tests + 1
  endif
end do

if (n_failed_tests > 0) then
  error stop n_failed_tests
endif

contains

subroutine test_flora(t)
  class(Test), intent(inout) :: t

  call t % fail("failure msg")
  return

end subroutine

end program 
