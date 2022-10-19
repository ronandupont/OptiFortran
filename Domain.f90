module Domain
  implicit none
  private
  public :: Dom

  type Dom
     real (kind = 8)                 :: xshore, xcoast, xstep, h0
  end type Dom
  
end module Domain