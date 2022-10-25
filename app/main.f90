program main
  implicit none

  type :: HexIndex
    integer :: x
    integer :: y
  end type

  integer, parameter :: DIR_N  = 1
  integer, parameter :: DIR_SW = 2
  integer, parameter :: DIR_NW = 3
  integer, parameter :: DIR_SE = 4
  integer, parameter :: DIR_NE = 5
  integer, parameter :: DIR_S  = 6
  integer, parameter :: N_DIRS = 6

  !         ___                 ___                                 
  !        /   \               /   \                              
  !    ___/  0  \___       ___/  1  \___                          
  !   /   \ +1  /   \     /   \     /   \                          
  !  / -1  \___/ +1  \   /  3  \___/  5  \                          
  !  \ +1  /   \  0  /   \     /   \     /       
  !   \___/  0  \___/     \___/  0  \___/              
  !   /   \  0  /   \     /   \     /   \                           
  !  / -1  \___/ +1  \   /  2  \___/  4  \                         
  !  \  0  /   \ -1  /   \     /   \     /                           
  !   \___/  0  \___/     \___/  6  \___/                           
  !       \ -1  /             \     /                                               
  !        \___/               \___/                                            
  !                                          

  type(HexIndex), parameter :: dir_vectors(0:6) = [ HexIndex( 0, 0), & ! 0
                                                    HexIndex( 0,+1), & ! 1
                                                    HexIndex(-1, 0), & ! 2
                                                    HexIndex(-1,+1), & ! 3
                                                    HexIndex(+1,-1), & ! 4
                                                    HexIndex(+1, 0), & ! 5
                                                    HexIndex( 0,-1)  ] ! 6

  integer, parameter :: N_CLOVER_VECS = 1
  integer, parameter :: N_BUNNY_VECS  = 7
  integer, parameter :: N_FOX_VECS    = 7
  integer, parameter :: N_VECS        = N_CLOVER_VECS + N_BUNNY_VECS + N_FOX_VECS

  integer, parameter :: CLOVER_BEGIN = 1
  integer, parameter :: CLOVER_END   = CLOVER_BEGIN + N_CLOVER_VECS - 1

  integer, parameter :: BUNNY_BEGIN = CLOVER_END + 1
  integer, parameter :: BUNNY_END   = BUNNY_BEGIN + N_BUNNY_VECS - 1

  integer, parameter :: FOX_BEGIN = BUNNY_END + 1 
  integer, parameter :: FOX_END   = FOX_BEGIN + N_FOX_VECS - 1

  integer, parameter :: NI = 3
  integer, parameter :: NJ = 3
  integer, parameter :: N_HEXAGONS = NI*NJ

  integer, parameter :: N_TIME_INDICES = 2

  real, parameter :: SATIATED_ENERGY = 100.0
  real, parameter :: DEATH = 0.0

  real, parameter :: CLOVER_CARRYING_CAPACITY = 100.0
  real, parameter :: CLOVER_GROWTH_RATE = 0.3

  real, parameter :: BUNNY_ESCAPE = 0.5
  real, parameter :: BUNNY_EAT = 10.0
  real, parameter :: BUNNY_MOVE_DRAIN = 2.0
  real, parameter :: BUNNY_START_PERCENT = 0.5
  real, parameter :: BUNNY_START_ENERGY = 100.0
  integer, parameter :: N_STARTING_BUNNIES = int( N_BUNNY_VECS * N_HEXAGONS * BUNNY_START_PERCENT )

  real, parameter :: FOX_EAT_CONVERSION = 0.6
  real, parameter :: FOX_MOVE_DRAIN = 3.0
  real, parameter :: FOX_START_PERCENT = 0.5
  real, parameter :: FOX_START_ENERGY = 100.0
  integer, parameter :: N_STARTING_FOXES = int( N_FOX_VECS * N_HEXAGONS * FOX_START_PERCENT )

  real,allocatable,target :: landscape(:,:,:,:)
  real, pointer :: clover (:,:,:,:)
  real, pointer :: bunnies(:,:,:,:)
  real, pointer :: foxes  (:,:,:,:)

  integer, parameter :: N_TIMESTEPS = 100

  integer :: cur_timestep, next_timestep

  cur_timestep = 1
  next_timestep = 2

  allocate( landscape(N_VECS, NI, NJ, N_TIME_INDICES) )

  clover  => landscape(CLOVER_BEGIN:CLOVER_END,:,:,:)
  bunnies => landscape(BUNNY_BEGIN:BUNNY_END  ,:,:,:)
  foxes   => landscape(FOX_BEGIN:FOX_END      ,:,:,:)

  landscape = 0.0

  ! Init the entire field to the carrying capacity
  clover = CLOVER_CARRYING_CAPACITY

  ! Now place foxes and bunnies

  block 
    integer :: i, ii, jj, kk
    do i = 1,N_STARTING_BUNNIES
      ii = uniform_random_int(1, NI)
      jj = uniform_random_int(1, NJ)
      kk = uniform_random_int(1, N_BUNNY_VECS)
      bunnies(kk,ii,jj,cur_timestep) = BUNNY_START_ENERGY
    end do
  end block

  block 
    integer :: i, ii, jj, kk
    real :: r
    do i = 1,N_STARTING_FOXES
      ii = uniform_random_int(1, NI)
      jj = uniform_random_int(1, NJ)
      kk = uniform_random_int(1, N_BUNNY_VECS)
      call random_number(r)
      foxes(kk,ii,jj,cur_timestep) = r*FOX_START_ENERGY
    end do
  end block

  block
    integer :: i, j
    do i = 1, NI
      do j = 1, NJ
        call update_foxes_in_cell(foxes(:,i,j,cur_timestep), &
                                  foxes(:,i,j,next_timestep), &
                                  bunnies(:,i,j,cur_timestep))
      end do
    end do
  end block


contains

  subroutine swap(foxes_cur, foxes_sort, a, b)
    real, intent(in) :: foxes_cur(N_FOX_VECS)
    integer, intent(inout) :: foxes_sort(N_FOX_VECS)
    integer, intent(in) :: a,b
    integer :: ai, bi, tmp
    ai = foxes_sort(a)
    bi = foxes_sort(b)

    if (foxes_cur(ai) < foxes_cur(bi)) then
      tmp = foxes_sort(a)
      foxes_sort(a) = foxes_sort(b)
      foxes_sort(b) = tmp
    end if
  end subroutine

  subroutine sort_foxes(foxes_cur, foxes_sort) 
    real, intent(in) :: foxes_cur(N_FOX_VECS)
    integer, intent(out) :: foxes_sort(N_FOX_VECS)
    integer :: j

    foxes_sort = [ 1,2,3,4,5,6,7 ]

    call swap(foxes_cur, foxes_sort, 2, 3)
    call swap(foxes_cur, foxes_sort, 4, 5)
    call swap(foxes_cur, foxes_sort, 6, 7)
    call swap(foxes_cur, foxes_sort, 1, 3)
    call swap(foxes_cur, foxes_sort, 4, 6)
    call swap(foxes_cur, foxes_sort, 5, 7)
    call swap(foxes_cur, foxes_sort, 1, 2)
    call swap(foxes_cur, foxes_sort, 5, 6)
    call swap(foxes_cur, foxes_sort, 3, 7)
    call swap(foxes_cur, foxes_sort, 1, 5)
    call swap(foxes_cur, foxes_sort, 2, 6)
    call swap(foxes_cur, foxes_sort, 1, 4)
    call swap(foxes_cur, foxes_sort, 3, 6)
    call swap(foxes_cur, foxes_sort, 2, 4)
    call swap(foxes_cur, foxes_sort, 3, 5)
    call swap(foxes_cur, foxes_sort, 3, 4)
  end subroutine

  subroutine update_foxes_in_cell(foxes_cur, foxes_next, bunnies_cur)
    real, intent(inout) :: foxes_cur(N_FOX_VECS)
    real, intent(inout) :: foxes_next(N_FOX_VECS)
    real, intent(inout) :: bunnies_cur(N_BUNNY_VECS)

    integer :: fox_vecs_left
    integer :: fox_vecs(N_FOX_VECS)
    integer :: foxes_sort(N_FOX_VECS)
    integer :: i,j,ii,bunnies_eaten,ri,tmp
    real :: r

    fox_vecs_left = N_FOX_VECS
    fox_vecs = [ 1, 2, 3, 4, 5, 6, 7 ]
    bunnies_eaten = 0

    call sort_foxes(foxes_cur, foxes_sort)

    i = 1
    do while (foxes_cur( foxes_sort(i) ) > 0) 
      ii = foxes_sort(i)

      do j=1,N_BUNNY_VECS
        if (bunnies_cur(j) > 0) then
          call random_number(r)
          if ( r >= BUNNY_ESCAPE ) then
            foxes_cur( ii ) = foxes_cur( ii ) + FOX_EAT_CONVERSION*bunnies_cur(j)
            bunnies_cur(j) = 0
            bunnies_eaten = bunnies_eaten + 1
          end if
        end if
      enddo

      call random_number(r)
      ri = int(r * fox_vecs_left)+1

      foxes_next(ri) = foxes_cur(ii)

      fox_vecs_left = fox_vecs_left - 1

      tmp = fox_vecs(ri)
      fox_vecs(ri) = fox_vecs(fox_vecs_left)
      fox_vecs(fox_vecs_left) = tmp

      i = i + 1

    end do

    write(*,*) "next",foxes_next
    write(*,*) "bunny cur", bunnies_cur
    write(*,*) "bunnies eaten",bunnies_eaten

  end subroutine

  function uniform_random_int(ibegin, iend) result(o)
    integer, intent(in) :: ibegin, iend
    integer :: ni,o
    real :: rn
    ni = iend-ibegin+1
    call random_number(rn)
    o = int( (rn*ni) ) + ibegin
  end function

end program main
