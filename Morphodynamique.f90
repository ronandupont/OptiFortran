module Morphodynamique
  implicit none
  private
  public :: Morpho

  type Morpho
     real (kind = 8), dimension(:),allocatable :: psi,psi0
	 
	 contains
	   procedure, public :: run_M
  end type Morpho
 
  contains

  subroutine run_M(this)
    class(Morpho), intent(inout) :: this
	integer                      :: i
	
	! DO i=1,this%nbtimestep
		! this%H1%current_time_step = i
		! this%H1%M1 = this%M1
		! CALL this%H1%run_H()
		! print*,"Running HydroMorpho code, time step:", i, "/", this%nbtimestep
	! ENDDO
	
	end subroutine run_M
	
end module Morphodynamique