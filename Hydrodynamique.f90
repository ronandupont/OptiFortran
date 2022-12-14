module Hydrodynamique
  use Morphodynamique
  use Domain
  implicit none
  private
  public :: Hydro

  type Hydro
     real (kind = 8), dimension(:),allocatable :: H,H00
     real (kind = 8)                           :: gamma, h0
	 Type(Morpho)                              :: M1
	 Type(Dom)                                 :: D
	 integer                                   :: current_time_step
	 
	 contains
	   procedure, public :: run_H
  end type Hydro
  
  contains

  subroutine run_H(this)
    class(Hydro), intent(inout)        :: this
	real (kind = 8), dimension(:),allocatable :: psi
	integer                            :: i
	real (kind = 8)                    :: H00,h0,gamma
	
	H00 = this%H00 (this%current_time_step)
	print*,H00
	psi= this%M1%psi
	gamma = this%gamma
	h0 = this%h0
	
	DO i= 1,this%D%xshore
	  IF (H00/(H0-psi(i))>gamma) THEN
	    this%H(i) = gamma * (H0-psi(i))
		print*,this%H(i)
	  ELSE
	    this%H(i) = H00
		print*,this%H(i)
		!print*,"coucou"
	  ENDIF
	ENDDO
	
	
	
	!print*,"test",this%H
	
  end subroutine run_H 
  
end module Hydrodynamique