module Hydrodynamique
  implicit none
  private
  public :: Hydro

  type Hydro
     real (kind = 8), dimension(:),allocatable :: H
  end type Hydro
  
end module Hydrodynamique