real(Ri_) :: A(N) = 0

A(1) = 1

do i = 2, N
   A(i) = A(i-1) + 1
end do

do i = 2, N
   A(i) = i 
end do

do i = 2, 4
   A(i) = i 
end do

A(1), A(2), A(3), A(4)

do i = 5, N, 4
   A(i:i+3) = A(i-4:i-1) + 4 
end do











