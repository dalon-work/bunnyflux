module testing
implicit none

private

type, public :: Test
  private
  logical :: succeeded_ = .true.
  character(:), allocatable :: msg_
contains
  procedure :: fail
  procedure :: failed
  procedure :: msg
end type

contains

subroutine fail(self, msg)
  class(Test), intent(inout) :: self
  character(*), intent(in) :: msg
  self % succeeded_ = .false.
  self % msg_ = msg
end subroutine

function failed(self)
  class(Test), intent(in) :: self
  logical :: failed
  failed = .not. self % succeeded_
end function

function succeeded(self)
  class(Test), intent(in) :: self
  logical :: succeeded
  succeeded =  self % succeeded_
end function

function msg(self)
  class(Test), intent(in) :: self
  character(:), allocatable :: msg
  if (.not. self % failed()) then
    msg = ""
  else
    msg = self % msg_
  end if
end function

end module

