program lab05
implicit none
   
   integer :: i ,ios, j, height, r, rows
   real :: s , e, increment, dif, t
   real,allocatable,dimension(:,:):: M
   character*20:: name = "" , last = "", header1 ="Centigrade", head2="Fahrenheit", head3="Kelvin"
   character*50:: file_name
   s = -11
   e = 211
   rows = 3 
  
print*, "Enter your first name(single word maximum 20 characters)"
read*, name
print*, "Enter your last name(single word maximum 20 characters)"
read*, last

 
do while(s<-10 .or. e>210)
  print*, "Enter the lower and upper temperature limit of your table(seperate with a comma)"
  read*, s, e
end do

t = s

dif = e - s
increment = 500
do while(increment<1 .or. increment>=dif)
  print*, "Enter a temperature increment(a value between 1 and", dif,")" 
  read*, increment
end do  
   
height =  NINT(ABS(e - s)/increment)
allocate(M(height, 3))
print*, "This table converts", s, "to", e, "celsius to Fahrenheit and Kelvin" 
do r = 1 , height
  do j = 1 , 3
    if (j > 2) then
      M(r , j) = s + 273.15
      else if (j > 1) then
        M(r , j) =  32.0 + 1.80 * s 
      else     
        M(r , j) = s
    end if    
  end do
  s = s + increment
end do
   file_name = name // '_' // last // ".txt"
   
   open(1, file = trim(file_name) ,form='formatted',ACCESS='Sequential',iostat=ios &
   ,status ='new', ERR=10)
   
   write(UNIT=1, FMT=102) t, e
   write(UNIT=1, FMT=103) height, rows
   write(unit=1, fmt=104) header1, head2, head3
   do i = 1, height
    write(1,100) M(i, 1), M(i, 2), M(i, 3)
   end do
   100 format (5x,f6.2,5x,f10.2, 5x, f12.2)
   102 format ("This table converts from ",T22, F10.2, " to ", F6.2, " Celsius to Fahrenheit and Kelvin")
   103 format ("This table has ",T15, i4, " rows and ", i1, " number of columns")
   104 format (5x, a11, 4x, a10, 7x, a6) 
   print*,ios  
   close(1)
10 print*,'text file created'   

end program lab05