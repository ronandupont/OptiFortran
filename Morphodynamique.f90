module Morphodynamique
  implicit none
  private
  public :: Morpho

  type Morpho
     real (kind = 8), dimension(:),allocatable :: psi,psi0
  end type Morpho
  
end module Morphodynamique