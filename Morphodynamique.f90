module Morphodynamique
  use Hydrodynamique
  use Domain
  implicit none
  private
  public :: Morpho

  type Morpho
     real (kind = 8), dimension(:),allocatable :: psi,psi0
	 Type(Dom)                                 :: D
	 Type(Hydro)                               :: H1
	 
	 contains
	   procedure, public :: run_M
  end type Morpho
 
  contains

  subroutine run_M(this)
    class(Morpho), intent(inout) :: this
	real (kind = 8), dimension(:),allocatable :: Lambda
	integer                      :: i
	
	! # Step 1 : calculate the sand excitation by the waves (and local abrasion ?)
	
	! # Step 2 : calculate the descent direction d with the conservation of energy projection (with or without the penalty term)
	
	! # Step 3 : Additive mobility:
	
	! # Step 4 : apply the descent method to psi (with or without the sand slope constraint)
	
	! # Step 5 : Additive mobility (bis):
	
	! # Step 6 :	Check bedrock
	
	! # Step 7 : Set the new seabed
	
	
	
	
	print*,"Coucou"
	
	! DO i=1,this%nbtimestep
		! this%H1%current_time_step = i
		! this%H1%M1 = this%M1
		! CALL this%H1%run_H()
		! print*,"Running HydroMorpho code, time step:", i, "/", this%nbtimestep
	! ENDDO
	
	end subroutine run_M


end module Morphodynamique