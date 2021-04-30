program q2
implicit none
real, dimension (3,3):: A, C
real, dimension (3,2):: B
integer:: i
A(1,1)=3.4
A(1,2)=-2.1
A(1.3)=0.0
A(2,1)=-11.0
A(2,2)=25.0
A(2,3)=-4.0
A(3,1)=5.2
A(3,2)=3.3
A(3,3)=-4.0
B(:1)=(/8.6, 11.3, 18)
B(:2)=(/12.0,-7.0,-10.7)
A=reshape((/3.4,-11.0,5.2,2.1,25.0,3.3,0.0,7.6,-4.0/),shape=(/3,3/))
B=reshape((/8.6,11.3,18.0,12.0,-7.0,-10.7/),shape=(/3,2/))
do i = 1, 3
  print*,A(i,:) 
end do
do i = 1, 3
  print*,B(i,:) 
end do
C = A*B
do i = 1,3
  print*, C(i,:)
end do
end program q2