module bunnyflux
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, bunnyflux!"
  end subroutine say_hello
end module bunnyflux
