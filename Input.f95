 program MidQ1
 
 implicit none 
 character (len = 10):: name 
 integer :: counter
 
 open (1, file = 'Problem1_input.txt')

 do counter = 1, 15 
   read(1,100) name
   Print*, name(1:3)   
 end do 

 100 format (a10)
 close(1);

 end program MidQ1