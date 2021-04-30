Program lab2
character*56:: name=""
integer interval
integer total
total = 0
print*, "This program prints all odd numbers between 0 and your desired number less than 1000."
print*, "Additionally, the program represents the sum of the odd numbers."
print*, "Please enter your name (maximum 65 letters)"
read*, name
print*,"Please enter your desire integers number( your number should be between 0 to 1000)"
read*, interval
do i = 1, interval, [2]
  print*, i
  total = total + i
end do
print*, "The sum of these odd numbers is:", total
print*, "Thank you " ,name, "for using this code."
end program lab2