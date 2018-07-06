module Text_IO
   use Environment

   implicit none
   
   ! Структура данных для хранения строки исходного текста.
   type TextLine
      character(:, CH_), allocatable  :: String
      type(TextLine),    pointer      :: Next   => Null()
   end type TextLine

contains
   ! Чтение исходного текста.
   function Read_Text(InputFile) result (Text)
      type(TextLine), pointer     :: Text
      character(*),   intent(in)  :: InputFile

      integer                     :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Text => Read_Text_Line(In)
      close (In)
   end function Read_Text

   ! Чтение строки исходного текста.
   recursive function Read_Text_Line(In) result(Text)
      type(TextLine), pointer     :: Text
      integer,        intent(in)  :: In

      integer,        parameter   :: MAX_LEN = 40
      character(MAX_LEN, CH_)     :: String
      integer                     :: IO

      ! Чтение строки во временную строку бОльшей длины.
      read (In, "(a)", iostat=IO) String
      call Handle_IO_Status(IO, "reading line from initial text")
      if (IO == 0) then
         allocate (Text)
         ! Хранение в размещаемом поле символов без завершающих пробелов.
         Text%String = Trim(String)
         Text%Next => Read_Text_Line(In)
      else
         Text => Null()
      end if
   end function Read_Text_Line
 
   ! Вывод текста.
   subroutine Output_Text(OutputFile, Text, Message, Access)
      character(*),              intent(in)  :: OutputFile
      type(TextLine),            intent(in)  :: Text
      character(:), allocatable, intent(in)  :: Message, Access

      integer                                :: Out, IO
      
      open (file=OutputFile, encoding=E_, newunit=Out, access=Access)
         if (Access == 'append') then
            write (Out, "(a)", iostat=IO)
            call Handle_IO_Status(IO, "writing line to file")
         end if
         write (Out, "(a)", iostat=IO) Message
         call Handle_IO_Status(IO, "writing line to file")
         call Output_Text_Line(Out, Text)
      close (Out)
   end subroutine Output_Text

   ! Вывод строки текста.
   recursive subroutine Output_Text_Line(Out, Text)
      integer,        intent(in)  :: Out
      type(TextLine), intent(in)  :: Text

      integer                     :: IO

      write (Out, "(a)", iostat=IO) Text%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Text%Next)) &
         call Output_Text_Line(Out, Text%Next)
   end subroutine Output_Text_Line
end module Text_IO
