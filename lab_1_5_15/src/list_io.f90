module List_IO
   use Environment

   implicit none
   
   ! Структура данных для хранения информации из строки списка:
   ! фамилии, имени и отчества.
   type ListLine
      character(:, CH_), allocatable  :: Surname
      character(:, CH_), allocatable  :: Name
      character(:, CH_), allocatable  :: Patronymic
      type(ListLine),    pointer      :: Next        => Null()
   end type ListLine

contains
   ! Чтение исходного списка.
   function Read_List(InputFile) result(List)
      type(ListLine), pointer     :: List
      character(*),   intent(in)  :: InputFile

      integer                     :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         List => Read_List_Line(In)
      close (In)
   end function Read_List

   ! Чтение строки исходного списка в виде фамилии, имени и отчества.
   recursive function Read_List_Line(In) result(List)
      type(ListLine), pointer     :: List
      integer,        intent(in)  :: In

      integer,        parameter   :: MAX_LEN = 42
      integer,        parameter   :: SURNAME_LEN = 15
      integer,        parameter   :: NAME_LEN = 10
      integer,        parameter   :: PATRONYMIC_LEN = 15
      character(MAX_LEN, CH_)     :: String
      integer                     :: IO

      ! Чтение строки во временную строку.
      read (In, "(a)", iostat=IO) String
      call Handle_IO_Status(IO, "reading line from initial text")
      if (IO == 0) then
         allocate (List)
         ! Хранение в размещаемых полях фамилии, имени и отчества.
         List%Surname = String(1:SURNAME_LEN)
         List%Name = String(SURNAME_LEN+2:SURNAME_LEN+NAME_LEN+1)
         List%Patronymic = String(SURNAME_LEN+NAME_LEN+3:SURNAME_LEN+NAME_LEN+PATRONYMIC_LEN)
         List%Next => Read_List_Line(In)
      else
         List => Null()
      end if
   end function Read_List_Line
 
   ! Вывод списка.
   subroutine Output_List(OutputFile, List, MessageLine, Access)
      character(*),              intent(in)  :: OutputFile
      type(ListLine),            intent(in)  :: List
      character(:), allocatable, intent(in)  :: MessageLine, Access

      integer                                :: Out, IO
      
      open (file=OutputFile, encoding=E_, newunit=Out, access=Access)
         if (Access == 'append') then
            write (Out, "(a)", iostat=IO)
            call Handle_IO_Status(IO, "writing line to file")
         end if
         write (Out, "(a)", iostat=IO) MessageLine
         call Handle_IO_Status(IO, "writing line to file")
         call Output_List_Line(Out, List)
      close (Out)
   end subroutine Output_List

   ! Вывод строки списка.
   recursive subroutine Output_List_Line(Out, List)
      integer,        intent(in)  :: Out
      type(ListLine), intent(in)  :: List

      integer                     :: IO

      write (Out, "(a)", iostat=IO) List%Surname // CH__" " // &
         List%Name // CH__" " // List%Patronymic
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(List%Next)) &
         call Output_List_Line(Out, List%Next)
   end subroutine Output_List_Line
end module List_IO
