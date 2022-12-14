!===========
program main 
!===========

! Modules
use HydroMorphodynamique
use forcings
use Domain

!-- Declarations
implicit none
integer               :: i,j,k,ii,jj
integer               :: n,ti,tf,n_x,xstep,dt,xshore,xcoast,nbtimestep
real (kind = 8) :: h0,t0,sandmobility,Mslope,loc,scal,Hmax,alpha,gamma, sigma, g
real (kind = 8), dimension(:),allocatable :: x,psi0,H00,time,mobility,slopemax,H
!Type(Hydro) ::  H1
!Type(Morpho) ::  M1
Type(HydroMorpho) ::  HM1
Type(Dom) ::  D

! Domain parameters
h0 = 7
xshore = 100 				! x coordinate of the shoreline
xstep = 1					! Discretization step of the domain
xcoast = 10					! Length of domain on-shore
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
gamma = 0.78                ! Munk criterous
sigma = 8.D0*DATAN(1.D0)/t0
g = 9.81

! Allocations
allocate (x(n_x),psi0(n_x),mobility(n_x),slopemax(n_x),H(n_x)) !dx
allocate (time(nbtimestep),H00(nbtimestep)) !dx

! Temporal initialisation
DO i=0,nbtimestep
time(i)=tf*float(i)/(nbtimestep)
ENDDO

CALL create_forcing(time, loc, scal, Hmax, alpha,H00)

! Spatial initialisation 
DO i=1,n_x
x(i) = i
psi0(i) = x(i) * h0 / xshore
mobility(i) = sandmobility
slopemax(i) = Mslope
ENDDO

HM1%gamma = gamma
HM1%sigma = sigma
HM1%g     = g

HM1%H00 = H00
HM1%H = H
HM1%H0 = H0
HM1%nbtimestep = nbtimestep-1499

HM1%psi0 = psi0
HM1%psi  = psi0

D%xcoast = xcoast
D%xshore = xshore
D%xstep  = xstep
D%h0     = h0

HM1%D = D


print*,"Init OK"

CALL HM1%run_HM()

print*,"Run OK"

end program
