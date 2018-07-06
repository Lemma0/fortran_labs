elemental function Cube(x)
   real(R_) Cube
   real(R_), intent(in) :: x

   Cube = x ** 3
end function

real(R_) :: A(N) = [(i, i = 1, N)], B(2*N) = 0
! A(1:10) == [1, 2, 3, 4, ..., 10]
! B(1:20) == [0, 0, 0, 0, ,,,, 0]

B(1:N:2) = Cube(A)
! B(1:20) == [1, 0, 8, 0, 27, 0, 64, 0, 125, 0, ...]


impure elemental function Products(x)
   real(R_) Products
   real(R_), intent(in) :: x

   real(R_) :: mem = 1

   mem = mem * x
   Products = mem
end function

! A(1:5) = Products([2, 2, 2, 2, 2,])
A(1:5) = Products([(2, i = 1, 5)])
! A(1:5) == [2, 4, 8, 16, 32]

A(1) = Products(2)
! mem = 1, x = 2, mem = 1 * 2 = 2, Products = 2
A(2) = Products(2)
! mem = 2, x = 2, mem = 2 * 2 = 4, Products = 4
A(3) = Products(2)
! mem = 4, x = 2, mem = 4 * 2 = 8, Products = 8
A(4) = Products(2)
A(5) = Products(2)

! Если первый запуск функции, т. е. mem = 1.
A(1:5) = Products([i, i = 1, 5])
A(1:5) = Products([1, 2, 3, 4, 5])
A(1) = Products(1)
! mem = 1, x = 1, mem = 1 * 1 = 1, Products = 1
A(2) = Products(2)
! mem = 1, x = 2, mem = 2 * 1 = 2, Products = 2
A(3) = Products(3)
! mem = 2, x = 3, mem = 2 * 3 = 6, Products = 6
A(4) = Products(4)
! mem = 6, x = 4, mem = 6 * 4 = 24, Products = 24
A(5) = Products(5)
! mem = 24, x = 5, mem = 24 * 5 = 120, Products = 120

