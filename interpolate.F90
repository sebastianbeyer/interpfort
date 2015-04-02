!! interpolation functions

program interpolate

  integer, parameter :: dp = kind(1.0d0)

  type :: index2d_t
    integer  :: i,j
  end type index2d_t

  integer    :: nx = 10
  integer    :: ny = 10

  real(dp), dimension(10)    :: x = [0,1,2,3,4,5,6,7,8,9]
  real(dp), dimension(10)    :: y = [0,1,2,3,4,5,6,7,8,9]

  real(dp), dimension(10,10) :: values

  integer    :: i,j
  type(index2d_t)   :: indexWant

  ! new grid
  real(dp), dimension(19)    :: xfine != [0.0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5]
  real(dp), dimension(19)    :: yfine != [0.0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5]


  ! set fine grid
  j = 1
  do i=1,size(xfine),2
    xfine(i) = x(j)
    xfine(i+1) = x(j)+0.5_dp
    j = j+1
  end do
  yfine = xfine

  print *,"interpolation test"

  !values = setValues(nx,ny)
    do i=1,nx
      do j=1,ny
        values(i,j) = i*i + j*j
      end do
    end do



  print *, "initial field:"
  print "(10 f8.2)", values

  print *, ""
  indexWant = findIndex2d (4.7_dp, 2.3_dp, x,y)

  print *, "x 4.7  i=", indexWant%i
  print *, "y 2.3  j=", indexWant%j



  print *, ""
  print *, "x   y   i    j"
    do i=1,size(xfine)
      do j=1,size(yfine)
        indexWant = findIndex2d (xfine(i), yfine(j), x,y)
        print *, xfine(i), yfine(j), indexWant%i, indexWant%j

      end do
    end do




contains

  pure function findIndex2d (xWant, yWant, x, y) result(indexWant)
    ! gets the *lower* index
    type(index2d_t)                     :: indexwant
    real(dp), intent(in)                :: xWant, yWant
    real(dp), intent(in), dimension(:)  :: x,y

    real(dp)      :: dx, dy
    integer       :: xIndex
    integer       :: yIndex

    dx = x(2) - x(1)
    dy = y(2) - y(1)

    xIndex = floor( (xWant - x(1))/dx )
    yIndex = floor( (yWant - y(1))/dy )

    indexWant%i = xIndex
    indexWant%j = yIndex

  end function findIndex2d

end program interpolate
