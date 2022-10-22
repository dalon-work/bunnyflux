module bunny_flux
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, bunny-flux!"
  end subroutine say_hello
end module bunny_flux
