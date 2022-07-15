program unittest
use iso_fortran_env, only: stderr => error_unit 
use testing
use flora
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
  real(r32) :: ans
  character(128) :: buf

  ans = flora_rk4(3.0, 0.0, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.1, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.2, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.3, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.4, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.5, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.6, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.7, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.8, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 0.9, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.0, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.1, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.2, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.3, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.4, 0.1)
  write(*,*) ans
  ans = flora_rk4(ans, 1.5, 0.5)
  write(*,*) ans

  if ( ans /= 0.0 ) then
    write(buf,*) "rk4 went wrong ", ans
    call t % fail(buf)
    return
  end if

end subroutine

end program 
