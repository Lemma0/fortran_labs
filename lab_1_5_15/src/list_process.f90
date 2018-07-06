! Модуль с ЧИСТЫМИ процедурами обработки списка.
module List_Process
   use Environment
   use List_IO

   implicit none

contains
   ! Функция прореживает список, удаляя из него строки с дублирующимися именами.
   ! Она нужна только для цели сохранения исходного списка, если в этом нет
   ! необходимости, можно обращаться напрямую к процедуре Dilute_Copy
   pure function Dilute_List(InitialList) result(DilutedList)
      type(ListLine), pointer     :: DilutedList
      type(ListLine), intent(in)  :: InitialList

      ! Поскольку невозможно модифицировать
      ! входной список, вначале его копирование.
      DilutedList => Copy_List(InitialList)
      ! Прореживание копии списка.
      call Dilute_Copy(DilutedList)
   end function Dilute_List

   ! Функция копирует список в буфер.
   pure recursive function Copy_List(InitialList) result(CopyOfList)
      type(ListLine), pointer     :: CopyOfList
      type(ListLine), intent(in)  :: InitialList

      allocate (CopyOfList)
      CopyOfList%Surname = InitialList%Surname
      CopyOfList%Name = InitialList%Name
      CopyOfList%Patronymic = InitialList%Patronymic
      ! Если еще остались строки, рекурсивный вызов функции.
      if (Associated(InitialList%Next)) &
         CopyOfList%Next => Copy_List(InitialList%Next)
   end function Copy_List

   ! Процедура прореживает копию списка, удаляя из него
   ! строки с дублирующимися именами.
   pure recursive subroutine Dilute_Copy(List)
      type(ListLine), pointer, intent(inout) :: List

      ! Удаление из списка строк с таким же именем
      call Shift_Lines(List%Name, List)
      ! Если еще остались строки, переход к следующей
      if (Associated(List%Next)) &
         call Dilute_Copy(List%Next)
   end subroutine Dilute_Copy

   ! Процедура сдвигает указатель со строки с
   ! совпадающим именем на следующую строку.
   pure recursive subroutine Shift_Lines(Name, List)
      character(:, CH_), allocatable, intent(in)    :: Name
      type(ListLine),    pointer,     intent(inout) :: List

      ! Если есть следующая строка,
      if (Associated(List%Next)) then
         ! и если в следующей строке такое же имя,
         if (Name == List%Next%Name) then
            ! то убираем из списка строку с повторяющимся именем.
            List%Next => List%Next%Next
            ! Повторяем.
            call Shift_Lines(Name, List)
         ! Если имена не совпадают,
         else
            ! то повторяем со следующей строкой.
            call Shift_Lines(Name, List%Next)
         end if
      endif
   end subroutine Shift_lines
end module List_process
