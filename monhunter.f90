program projectile_motion
implicit none

integer::i
real::vb,vxb,vyb,xb,yb,theta,h,xm,ym,t,x0,y0,r,thetarad,d,vxb0,vyb0,xb0,yb0
real,parameter::pi=acos(-1.0)
!real,dimension(1:1000)::x,y
print*,"give vb,h,d"
read*,vb,h,d

r=h/d
thetarad=atan(r)
theta = 180*thetarad/pi

write(*,*)theta

vxb0=vb*cos(thetarad)
vyb0=vb*sin(thetarad)

t=d/vxb0

!vb=sqrt(vxb**2+vyb**2)

write(*,*)t

xb0=0
yb0=0
xm=d
ym=h

open(1,file="projectile2.dat")
t=0
do i=0,600
xb=vxb0*t+xb0
yb=vyb0*t-0.5*9.8*t**2+yb0

xm=d
ym=h-0.5*9.8*t**2

! if(mod(i,2)==0)then
 write(1,*)xb,yb,xm,ym,t
! end if
 
t=t+0.1
end do

end program
