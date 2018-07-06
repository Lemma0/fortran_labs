! Программа “центрирования” строк заданного текста, каждая из которых
! имеет длину L≤40 символов и строки имеют разное число пробелов слева.
! Во входном файле F1 находится заданный текст, а в файле F2 — тот же
! текст, но центрированный.
program lab_2_15
   use Environment
   use Text_Process
   use Text_IO

   implicit none
   character(:),   allocatable  :: F1, F2, InitialAccess, CenteredAccess, InitialMessage, CenteredMessage

   type(TextLine), pointer      :: InitialText  => Null()   ! Первоначальный текст.
   type(TextLine), pointer      :: CenteredText => Null()   ! Центрированный текст.

   F1 = "data/original_text.txt"
   F2 = "centered_text.txt"
   InitialAccess   = 'sequential'
   CenteredAccess  = 'append'
   InitialMessage  = 'Начальный текст:'
   CenteredMessage = 'Центрированный текст:'

   InitialText => Read_Text(F1)

   if (Associated(InitialText)) then
      call Output_Text(F2, InitialText, InitialMessage, InitialAccess)
      CenteredText => Center_Text(InitialText)
      if (Associated(CenteredText)) &
         call Output_Text(F2, CenteredText, CenteredMessage, CenteredAccess)
   end if

end program lab_2_15
