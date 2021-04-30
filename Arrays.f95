 module RandMatrixArray
 implicit none
 
 contains
 
 function RandMatrix(rows,columns) result(matrix)
 implicit none
 integer::rows,columns,i,j, val
 real:: matrix(rows,columns)
 do i = 1,rows
  do j = 1,columns
    val = time()
   matrix(i,j)=real(mod(val,5))*real(i*j)/real(i+j)
  end do
 end do
 
 end function RandMatrix 

 subroutine Array(x,AVG)
 implicit none
 real, dimension (:,:), intent(in):: x ! maximum array
 integer:: s(2)
 real, intent(out)::AVG
 s = shape(x)
 AVG = sum(x)/real(s(1)*s(2))
 end subroutine Array

 end module 


 Program Lab08
 use RandMatrixArray

 implicit none
 real, allocatable :: a(:,:)
 real::average
 integer:: n,m,i
 print*, 'This program creates a nxm matrix using a random function'
 print*, 'Then displays the average value of the matrix elements on the screen' 
 print*, 'Enter n and m respectively (comma separated)'
 read(*,*) n,m 
 ! creation of nxm random matrix
 allocate(a(n,m))
 a = RandMatrix(n,m)
 print*, 'the generated matrix is'
 do i=1,n
  write(*,*) a(i,:)
 end do
 Call Array(a,average)
 print*, 'the average value of the matrix elements is:', average

end program Lab08