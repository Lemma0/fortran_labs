! Модуль с ЧИСТЫМИ процедурами обработки данных.
module List_Process
   use Environment
   use List_IO

   implicit none

contains
   ! Создание маски для прореживания списка.
   pure function Create_Mask(InitialList) result(Mask)
      logical, dimension(LIST_LEN)   :: Mask
      type(student), intent(in)      :: InitialList(LIST_LEN)

      character(NAME_LEN, kind=CH_)  :: UniqueNames(LIST_LEN)
      integer                        :: i, j, k
      logical                        :: HasMet

      ! Составление маски строк с уникальными именами.
      Mask = .true.
      ! Минимальное количество уникальных имен.
      k = 1
      ! Первое уникальное имя - из первой строки списка.
      UniqueNames(1) = InitialList(1)%Name
      ! Опрашивание каждого последующего имени в списке.
      do i = 2, LIST_LEN
         ! Проверка, не присутствует ли имя из текущей строки в списке уникальных имен.
         HasMet = .false.
         do j = 1, k
            if (InitialList(i)%Name == UniqueNames(j)) then
               HasMet = .true.
            end if
         end do
         ! Если оно встречалось, то записываем об этом информацию в маску.
         if (HasMet) then
            Mask(i) = .false.
         ! Если не встречалось, добавляем имя к списку уникальных имен.
         else
            k = k + 1
            UniqueNames(k) = InitialList(i)%Name
         end if
      end do
   end function Create_Mask
end module List_process
