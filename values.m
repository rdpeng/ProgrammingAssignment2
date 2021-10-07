function [x1,y1,z1] = values(Volume,xcenter,ycenter,zcenter)
%Input the value for Volume and Center of Sphere
%Solve for radius with variable r
[x,y,z] = sphere;
 r = (3*(Volume/4*pi))^(1/3);
 x1=r*x+xcenter;
 y1=r*y+ycenter;
 z1=r*z+zcenter;
 
 surf(r*x+xcenter,r*y+ycenter,r*z+zcenter)
 xlabel('enter label for x')
 ylabel('enter label for y')
 zlabel('enter label for z')

end

