module newton
 implicit none 

 contains 

 subroutine solve(f1,f2,x0,n, EPSILON, x, iteration)
 implicit none
 
 real(kind = 2), external:: f1, f2
 real (kind = 2), intent(in):: x0, EPSILON
 real (kind = 2), intent(out):: x
 integer, intent(in):: n
 integer, intent(out):: iteration
 iteration = 0
 x = x0

 do while (abs(f1(x))>EPSILON)
   iteration = iteration + 1
   x = x - (f1(x)/f2(x))
   print*, iteration, x, f1(x)
   if (iteration >= n) then 
     print*, "No Convergence"
     stop
   end if  
 end do
 
 end subroutine solve

 
 real (kind = 2) function f(x)  ! this is f(x)
 implicit none
 real (kind = 2), intent(in)::x
 f = x**2.0d0-1.0d0
 end function f

 real (kind = 2) function fp(x)  ! This is f'(x)
 implicit none
 real (kind = 2), intent(in)::x
 fp = 2.0d0*x
 end function fp

end module newton   

Program Lab10
 use newton
 implicit none
 integer, parameter :: n = 1000  ! maximum iteration
 real(kind = 2), parameter :: EPSILON = 1.d-3
 real(kind = 2):: x0, x
 integer :: iteration 
 x0 = 3.0d0

 call solve(f,fp,x0,n, EPSILON, x, iteration) 
 print*,"The number of Iterations is:", iteration , " and the x-intercept is :", x
 
end program Lab10