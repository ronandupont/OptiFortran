module HydroMorphodynamique
  use Hydrodynamique
  use Morphodynamique
  use Domain

  implicit none
  private
  public :: HydroMorpho,run_HM

  type HydroMorpho
     Type(Hydro)  :: H1
	 Type(Morpho) :: M1
	 integer      :: nbtimestep
	 
	 contains
	   procedure, public :: run_HM
  end type HydroMorpho
  
  contains

  subroutine run_HM(this)
    class(HydroMorpho), intent(inout) :: this
	integer                       :: i
	
	DO i=1,this%nbtimestep
		this%H1%current_time_step = i
		this%H1%M1 = this%M1
		CALL this%H1%run_H()
		print*,"Running HydroMorpho code, time step:", i, "/", this%nbtimestep
	ENDDO
	
  end subroutine run_HM

end module HydroMorphodynamique