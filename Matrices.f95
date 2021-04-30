program lab4
implicit none 
logical :: t
real (kind=2):: s, e, increment
real,allocatable,dimension(:,:):: M
integer:: height, j, k , i
increment = 250
t = .true. 
do while(t) 
  print*, "Enter the lower and upper temperature limit of your table(seperate with a comma)"
  read*, s,e   
 if (s >= -10)then
    if (e <= 210)then
      exit
    else
      print*, "Please enter values again"
 end if
   else
   print*, "Please enter values again"   
  end if
end do   
do while (increment>ABS(e - s))
  print*, "Enter a temperature increment"
  read*, increment
end do 
height =  NINT(ABS(e - s)/increment)
allocate(M(height, 3))
do i = 1 , height
  do j = 1 , 3
    if (j > 2) then
      M(i , j) = s + 273.15
      else if (j > 1) then
        M(i , j) =  32.0 + 1.80 * s 
      else     
        M(i , j) = s
    end if    
  end do
  s = s + increment
end do          
do k = 1, height
  print*, M(k,:)
end do  
print*, "This is the size of the table:", height , " by",3
Print*, "Thank you",
end program lab4