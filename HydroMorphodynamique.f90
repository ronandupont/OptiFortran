module HydroMorphodynamique
  use Domain

  implicit none
  private
  public :: HydroMorpho,run_HM

  type HydroMorpho
	 integer                                   :: nbtimestep
     real (kind = 8), dimension(:),allocatable :: H,H00
	 real (kind = 8), dimension(:),allocatable :: psi,psi0
     real (kind = 8)                           :: gamma, sigma, h0, g
	 Type(Dom)                                 :: D
	 integer                                   :: current_time_step
	 
	 
	 contains
	   procedure, public :: run_HM, run_H, run_M
  end type HydroMorpho
  
  contains

  subroutine run_HM(this)
    class(HydroMorpho), intent(inout) :: this
	integer                       :: i
	
	DO i=1,this%nbtimestep
		CALL this%run_H()
		CALL this%run_M()
		print*,"Running HydroMorpho code, time step:", i, "/", this%nbtimestep
	ENDDO
	
  end subroutine run_HM
  
  
  subroutine run_H(this)
    class(HydroMorpho), intent(inout)        :: this
	real (kind = 8), dimension(:),allocatable :: psi
	integer                            :: i
	real (kind = 8)                    :: H00,h0,gamma
	
	H00 = this%H00 (this%current_time_step+1)
	!H00 = 4
	psi= this%psi
	gamma = this%gamma
	h0 = this%h0
	
	DO i= 1,this%D%xshore
	  IF (H00/(H0-psi(i))>gamma) THEN
	    this%H(i) = gamma * (H0-psi(i))
		print*,this%H(i)
	  ELSE
	    this%H(i) = H00
		print*,this%H(i)
	  ENDIF
	ENDDO
	print*,"ok"
  end subroutine run_H 
	
  subroutine run_M(this)
    class(HydroMorpho), intent(inout)        :: this
	real (kind = 8), dimension(:),allocatable :: psi, Lambda, k,  h
	integer                            :: i
	real (kind = 8)                    :: H00, h0, gamma, sigma, g
	
	h = this%h0 - this%psi
	sigma = this%sigma 
	g = this%g
	CALL k_guo(h, sigma, g, k)

	! # Step 1 : calculate the sand excitation by the waves (and local abrasion ?)
	CALL agitation(h, k, Lambda)
	print*,Lambda
	! # Step 2 : calculate the descent direction d with the conservation of energy projection (with or without the penalty term)
	
	! # Step 3 : Additive mobility:
	
	! # Step 4 : apply the descent method to psi (with or without the sand slope constraint)
	
	! # Step 5 : Additive mobility (bis):
	
	! # Step 6 :	Check bedrock
	
	! # Step 7 : Set the new seabed
  end subroutine run_M 	
	
  subroutine V_orb(h, k, h0, z, y)
	real (kind = 8), dimension (:), allocatable, intent(out) :: y
    real (kind = 8), dimension (:), allocatable, intent(in) :: h, k, h0, z
	
	y = cosh(k*(h - h0 +z))/cosh(k*h) 
	
  end subroutine V_orb
  
  subroutine agitation(h, k, y)
	real (kind = 8), dimension (:), allocatable, intent(out) :: y
    real (kind = 8), dimension (:), allocatable, intent(in) :: h, k
	
	y = 1.d0/cosh(k*h) 
	
  end subroutine agitation
  
  subroutine cut_n(x, y)
  	integer                            :: i
	real (kind = 8), dimension (:), allocatable, intent(inout) :: y
    real (kind = 8), dimension (:), allocatable, intent(inout) :: x
	y = x
	DO i=1,SIZE(x)
	  IF ((x(i)<0).OR.(isnan(x(i)))) THEN
	    y(i) = 0
	  ENDIF
	ENDDO
	
  end subroutine cut_n
  
  subroutine k_guo(h, sigma, g, y)
	real (kind = 8), dimension (:), allocatable, intent(inout)   :: y
	real (kind = 8), dimension (:), allocatable, intent(inout) :: h
    real (kind = 8)                                            :: sigma, g
	real (kind = 8), dimension(:),allocatable                  :: x
	real (kind = 8)                                            :: beta
	
	CALL cut_n(h,h)
	x = h * sigma / sqrt( g*h )
	beta = 2.4901	
	y = x**2/h*(1-exp(-x**beta))**(-1/beta)
	CALL cut_n(y,y)
	
  end subroutine k_guo
  
end module HydroMorphodynamique