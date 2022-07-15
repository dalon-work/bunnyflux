! Implements the edible flora equation
! which is the logistics equation
! https://mathworld.wolfram.com/LogisticEquation.html
! 
! dN/dt = r*N*(K-N)/K - C
! 
! Divide both sides by K and define x = N/K
!
! dx/dt = r*x*(1-x) - C/K
!
! This ode can be integrated in time using whatever method you find reasonable.
! C is a consumption term - how much gets eaten by the bunnies

module flora
use iso_fortran_env, only : r32 => real32
implicit none

real(r32), parameter :: growth_rate = 0.3
real(r32), parameter :: carrying_capacity = 10.0
real(r32), parameter :: consumption_rate = 0.0

contains

pure elemental function flora_rhs(time, pop)
  real(r32), intent(in) :: time, pop
  real(r32) :: flora_rhs

  flora_rhs = growth_rate * pop * (1.0 - pop / carrying_capacity) - consumption_rate
end function

pure elemental function flora_rk4(pop0,t0,dt)
  real(r32), intent(in) :: pop0, t0, dt
  real(r32) :: flora_rk4, k1, k2, k3, k4

  k1 = flora_rhs(t0+dt, pop0)
  k2 = flora_rhs(t0+0.5*dt, pop0+0.5*dt*k1)
  k3 = flora_rhs(t0+0.5*dt, pop0+0.5*dt*k2)
  k4 = flora_rhs(t0+dt, pop0+dt*k3)
  flora_rk4 = pop0 + (1.0/6.0)*(k1+2*k2+2*k3+k4)*dt

end function

end module


