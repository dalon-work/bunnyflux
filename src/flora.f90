! Implements the flora equation
! Which is the logistics equation
! dN/dt = r*N*(K-N)/K
module flora

contains

pure elemental function flora_rhs(r,N,K)
  real, intent(in) :: r,N,K
  real :: flora_rhs
  flora_rhs = r*N*(K-N)/K
end function

end module


