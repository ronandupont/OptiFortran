!==================
module forcings
!==================
!
contains

subroutine npdf(x,y)
  real (kind = 8), dimension (:), allocatable, intent(out) :: y
  real (kind = 8), dimension (:), allocatable, intent(in) :: x
  
    y = 1/(2*3.14159)**0.5*dexp(-x**2/2)
	
  return
end subroutine npdf


subroutine cdf(x,y)
  real (kind = 8), dimension (:), allocatable, intent(out) :: y
  real (kind = 8), dimension (:), allocatable, intent(in) :: x
  
    y = 0.5*(1+erf(x/2**0.5))
	
  return
end subroutine cdf


subroutine pdf(x,alpha,y)
  real (kind = 8), intent(in) :: alpha
  real (kind = 8), dimension (:), allocatable, intent(out) :: y
  real (kind = 8), dimension (:), allocatable, intent(in) :: x
  real (kind = 8), dimension(:),allocatable :: terme_1,terme_2,terme_3
  
  call npdf(x,terme_1)
  terme_2 = x * alpha
  call cdf(terme_2,terme_3)
   
  y = terme_1*terme_3*2
 
  return
end subroutine pdf


subroutine create_forcing(time, loc, scal, hmax, alpha,y)
 real (kind = 8), intent(in) :: loc, scal, hmax, alpha
 real (kind = 8) :: M
 real (kind = 8), dimension (:), allocatable, intent(in) :: time
 real (kind = 8), dimension (:), allocatable, intent(out) :: y

  real (kind = 8), dimension(:),allocatable :: r,arg_1,arg_2,terme_1,terme_2,terme_3
  
  arg_1=(time-loc)/scal
  call npdf(arg_1,terme_1)
  
  arg_2= alpha*(time-loc)/scal
  call cdf(arg_2,terme_2)
  r = 2/scal*terme_1*terme_2
  M = maxval(r)+0.0000001
  
  y = hmax/M*r+0.1

  return
end subroutine create_forcing








!======================
end module forcings
!======================