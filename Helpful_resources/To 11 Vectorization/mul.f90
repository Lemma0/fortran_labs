real(R_) :: A(K, M), B(M, N), C(K, N)

do i = 1, K
   do j = 1, N
      do l = 1, M
         C(i, j) = C(i, j) + A(i, l)*B(l, j)
      end do
   end do
end do

do i = 1, K
   do j = 1, N
      C(i, j) =Sum(A(i, 1:M)*B(1:M, j))
   end do
end do
