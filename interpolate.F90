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
  real(dp), dimension(19,19) :: valuesfine

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
  print "(10 f10.1)", values

  print *, ""
  ! indexWant = findIndex2d (4.7_dp, 2.3_dp, x,y)

  ! print *, "x 4.7  i=", indexWant%i
  ! print *, "y 2.3  j=", indexWant%j



  print *, ""
  ! print *, "x   y   i    j"
    do i=1,size(xfine)
      do j=1,size(yfine)
        ! indexWant = findIndex2d (xfine(i), yfine(j), x,y)
        ! print *, xfine(i), yfine(j), indexWant%i, indexWant%j
        valuesfine(i,j) = bilinear_interp (xfine(i), yfine(j), x, y, values)
      end do
    end do

  print *,"finefield:"
  print "(19 f6.1)", valuesfine




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

    indexWant%i = xIndex+1
    indexWant%j = yIndex+1

  end function findIndex2d

  pure function bilinear_interp (xWant, yWant, x,y, coarseGrid) result(value)
    ! as found on wikipedia
    real(dp)                              :: value
    real(dp), intent(in)                  :: xWant, yWant
    real(dp), intent(in), dimension(:)    :: x,y
    real(dp), intent(in), dimension(:,:)  :: coarseGrid

    type(index2d_t)                       :: indexWant
    real(dp)                              :: x1,x2,y1,y2
    integer                               :: i1,i2,j1,j2

    indexWant = findIndex2d (xWant, yWant, x,y)
    i1 = indexWant%i
    i2 = i1+1
    j1 = indexWant%j
    j2 = j1+1

    x1 = x(i1)
    x2 = x(i2)
    y1 = y(j1)
    y2 = y(j2)

    ! print *, i1,i2,j1,j2
    ! print *, x1, x2, y1, y2

    ! see http://en.wikipedia.org/wiki/Bilinear_interpolation
    value = 1.0_dp / ((x2-x1)*(y2-y1)) * ( coarseGrid(i1,j1) * (x2-xWant)*(y2-yWant) + &
                                          & coarseGrid(i2,j1) * (xWant-x1)*(y2-yWant) + &
                                          & coarseGrid(i1,j2) * (x2-xWant)*(yWant-y1) + &
                                          & coarseGrid(i2,j2) * (xWant-x1)*(yWant-y1) )




  end function bilinear_interp

end program interpolate
