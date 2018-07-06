! Модуль с ЧИСТЫМИ процедурами обработки данных.
module List_Process
   use Environment
   use List_IO

   implicit none

contains
   ! Создание маски для прореживания списка.
    function Create_Mask(InitialList) result(Mask)
      logical, dimension(LIST_LEN)   :: Mask
      type(student), intent(in)      :: InitialList(LIST_LEN)

      character(NAME_LEN, kind=CH_)  :: UniqueNames(LIST_LEN)
      integer                        :: i, k

      ! Составление маски строк с уникальными именами.
      ! Вначале предполагаем, что все имена в списке уникальны.
      Mask = .true.
      ! Минимальное количество уникальных имен.
      k = 1
      ! Первое уникальное имя - из первой строки списка.
      UniqueNames(1) = InitialList(1)%Name
      ! Опрашивание каждого последующего имени в списке.
      i = 2; call Check_List(i, k, InitialList, UniqueNames, Mask)
   end function Create_Mask

   ! Опрашивание каждого имени в списке, кроме первого.
   pure recursive subroutine Check_List(i, k, InitialList, UniqueNames, Mask)
      integer,                       intent(inout)  :: i, k
      type(student),                 intent(in)     :: InitialList(LIST_LEN)
      character(NAME_LEN, kind=CH_), intent(inout)  :: UniqueNames(LIST_LEN)
      logical, dimension(LIST_LEN),  intent(out)    :: Mask

      integer :: j

      ! Проверка, не присутствует ли имя из текущей строки в списке уникальных имен.
      j = 1
      call Check_Uniqueness(j, k, InitialList(i)%Name, UniqueNames, Mask(i))
      ! Повторяем процедуру со следующей строкой до конца списка
      if (i < LIST_LEN) then
         i = i + 1
         call Check_List(i, k, InitialList, UniqueNames, Mask)
      end if
   end subroutine Check_List

   ! Проверка имени на наличие в списке уникальных имен
   pure recursive subroutine Check_Uniqueness(j, k, Name, UniqueNames, Unique)
      integer,                       intent(inout)  :: j, k
      character(NAME_LEN, kind=CH_), intent(in)     :: Name
      character(NAME_LEN, kind=CH_), intent(inout)  :: UniqueNames(LIST_LEN)
      logical,                       intent(inout)  :: Unique

      ! Если запрашиваемое имя совпадает с именем из списка уникальных имен на
      ! текущей позиции, то, следовательно, оно уже встречалось.
      if (Name == UniqueNames(j)) then
         Unique = .false.
      end if
      ! Если имя нашлось среди уникальных имен, не имеет смысла просматривать
      ! список уникальных имен дальше.
      if (.not. Unique) j = k
      ! Если опрошен весь список уникальных имен, а запрашиваемое имя в нем так
      ! и не нашлось, список уникальных имен расширяетя добавлением в него
      ! запрашиваемого имени.
      if ((Unique) .and. j == k) then
         k = k + 1
         UniqueNames(k) = Name
         j = k
      end if
      ! Если еще не весь список уникальных имен опрошен, повторяем запрос со
      ! следующим именем из списка уникальных имен.
      if (j < k) then
         j = j + 1
         call Check_Uniqueness(j, k, Name, UniqueNames, Unique)
      end if
   end subroutine Check_Uniqueness
end module List_process
