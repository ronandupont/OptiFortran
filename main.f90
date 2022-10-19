!===========
program main 
!===========

! Modules
use Hydrodynamique
use Morphodynamique
use HydroMorphodynamique
use forcings

!-- Declarations
implicit none
integer               :: i,j,k,ii,jj
integer               :: n,ti,tf,n_x,xstep,dt,xshore,xcoast,nbtimestep
real (kind = 8) :: h0,t0,sandmobility,Mslope,loc,scal,Hmax,alpha
real (kind = 8), dimension(:),allocatable :: x,psi0,H,time,mobility,slopemax
!Type(Hydro) ::  H1
!Type(Morpho) ::  M1
Type(HydroMorpho) ::  HM1

! Domain parameters
h0 = 7
xshore = 600 				! x coordinate of the shoreline
xstep = 1					! Discretization step of the domain
xcoast = 30					! Length of domain on-shore
n_x = xcoast+xshore

! Sediment parameters
sandmobility = 4.2e-5			! Sand mobility
Mslope = 0.2				! Maximal sand slope

! Time parameters
ti = 0					! Starting time
tf = 1.8*10**6				! Final time
nbtimestep = 1500             ! Number of time incrementations

! Forcing parameters
loc = float(tf)/2				! Position of storm apex
scal = 1*10**5				! Storm width parameter
Hmax = 2					! Storm apex height
alpha = -4 					! Storm skewness parameter   
t0 = 2                      ! Wave period

! Allocations
allocate (x(n_x),psi0(n_x),mobility(n_x),slopemax(n_x)) !dx
allocate (time(nbtimestep),H(nbtimestep)) !dx

! Temporal initialisation
DO i=0,nbtimestep
time(i)=tf*float(i)/(nbtimestep)
ENDDO

CALL create_forcing(time, loc, scal, Hmax, alpha,H)

! Spatial initialisation 
DO i=0,n_x
x(i) = i
psi0(i) = x(i) * h0 / xshore
mobility(i) = sandmobility
slopemax(i) = Mslope
ENDDO

HM1%H1%H = H
HM1%nbtimestep = nbtimestep

print*,"Init OK"

CALL run(HM1)



end program
