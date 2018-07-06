! Модуль с ЧИСТЫМИ процедурами обработки текста.
module Text_Process
   use Environment
   use Text_IO

   implicit none

contains
   ! Формирование центрированного текста в виде новых строк.
   pure function Center_Text(InitialText) result(CenteredText)
      type(TextLine), pointer     :: CenteredText
      type(TextLine), intent(in)  :: InitialText

      type(TextLine), pointer     :: PreprocessedText
      integer                     :: MaxLen

      ! Подготовка текста: удаление пробелов в начале и конце строки
      PreprocessedText => Preprocess_Text(InitialText)

      ! Вычисление максимальной длины строки в подготовленном тексте
      MaxLen = 0
      call Max_Length(PreprocessedText, MaxLen)

      ! Окончательное выравнивание текста по центру
      CenteredText => Finalize_Text(PreprocessedText, MaxLen)
   end function Center_Text

   ! Функция подготавливает текст, удаляя из него пробелы в начале и конце строк
   pure recursive function Preprocess_Text(InitialText) result(PreprocessedText)
      type(TextLine), pointer     :: PreprocessedText
      type(TextLine), intent(in)  :: InitialText

      allocate (PreprocessedText)
      ! Удаление из строки пробелов слева и справа
      PreprocessedText%String = Trim(AdjustL(InitialText%String))

      ! Если еще остались строки, рекурсивный вызов функции
      if (Associated(InitialText%Next)) &
         PreprocessedText%Next => Preprocess_Text(InitialText%Next)
   end function Preprocess_Text

   ! Процедура подсчитывает длину самой длинной строки в тексте
   pure recursive subroutine Max_Length(Text, MaxLen)
      integer, intent(inout)  :: MaxLen
      type(TextLine), pointer :: Text

      ! Обновляем выходную переменную MaxLen, если текущая строка длиннее
      MaxLen = Max(MaxLen, Len(Text%String))
      ! Если еще остались строки, переход к следующей
      if (Associated(Text%Next)) &
         call Max_Length(Text%Next, MaxLen)
   end subroutine Max_Length

   ! Функция окончательно производит центрирование текста
   pure recursive function Finalize_Text(PreprocessedText, MaxLen) result(CenteredText)
     type(TextLine),    pointer      :: CenteredText
     type(TextLine),    intent(in)   :: PreprocessedText
     integer,           intent(in)   :: MaxLen

     integer                         :: I,N
     character(:, CH_), allocatable  :: Tabulation

     ! Формирование отступа
     Tabulation = ""
     N = (MaxLen - Len(PreprocessedText%String))/2
     if (N > 0) then
        do I=1,N
           Tabulation = CH__" " // Tabulation
        end do
     end if
     allocate (CenteredText)
     ! Добавление пробелов слева
     CenteredText%String = Tabulation // PreprocessedText%String
     ! Если еще остались строки, рекурсивный вызов функции
     if (Associated(PreprocessedText%Next)) &
        CenteredText%Next => Finalize_Text(PreprocessedText%Next, MaxLen)
   end function Finalize_Text
end module Text_process
