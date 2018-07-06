program Arthur
   real(R_) :: A(5)
   real(R_), allocatable :: B(:)

   A = Allocate_Array(5) ! X
   A = Allocate_Array(4) ! V
   A = Allocate_Array(6) ! X

   B = Allocate_Array(5) ! V 
   B = Allocate_Array(4) ! V 
   B = Allocate_Array(6) ! V 

contains
   function Allocate_Array(N) result(Allocate)
      real(R_), allocatable :: Allocate(:)
   
      allocate (Allocate(N+1))
   end function Allocate_Array
end program

