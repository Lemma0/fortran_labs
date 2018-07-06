function F(x, N, Elements)
   real(R_)    F ! , x
   real(R_)    x
   integer(I_) N
   real(R_)    Elements(:)
   integer(I_)              k  

   do concurrent (integer :: k = 1:N)
      Elements(k) = Sin(k*x) / k
   end do

   F = Sum(Elements)
end function

function F(x, N)
   real(R_)    F ! , X
   real(R_)    X
   integer(I_) N

   real(R_), allocatable :: Elements(:)
   real(R_) :: Some_Elements(N)
   integer(I_)              k  

   ! Elements = [(Sin(k*x) / k, k = 1, N)]

   allocate (Elements(N))
   do concurrent (integer :: k = 1:N)
      Elements(k) = Sin(k*x) / k
   end do

   ! allocate (Elements(N))
   ! forall (k = 1:N) &
   !    Elements(k) = Sin(k*x) / k

   F = Sum(Elements)
end function
