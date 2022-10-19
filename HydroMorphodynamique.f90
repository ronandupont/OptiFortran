module HydroMorphodynamique
  use Hydrodynamique
  use Morphodynamique

  implicit none
  private
  public :: HydroMorpho, run

  type HydroMorpho
     Type(Hydro)  :: H1
	 Type(Morpho) :: M1
	 integer      :: nbtimestep
  end type HydroMorpho
  
  contains

  subroutine run(this)
    type(HydroMorpho), intent(in) :: this
    type(Hydro) :: H2
	integer                       :: i
	
	DO i=0,this%nbtimestep
		print*,"Running HydroMorpho code, time step:", i, "/", this%nbtimestep
	ENDDO
	
  end subroutine run

end module HydroMorphodynamique