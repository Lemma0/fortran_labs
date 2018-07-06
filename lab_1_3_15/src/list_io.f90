module List_IO
   use Environment

   implicit none
   integer, parameter :: LIST_LEN       = 15
   integer, parameter :: SURNAME_LEN    = 15
   integer, parameter :: NAME_LEN       = 10
   integer, parameter :: PATRONYMIC_LEN = 15

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)     :: Surname    = ""
      character(NAME_LEN, kind=CH_)        :: Name       = ""
      character(PATRONYMIC_LEN, kind=CH_)  :: Patronymic = ""
   end type student
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(InputFile, DataFile)
      character(*), intent(in)   :: InputFile, DataFile
      
      type(student)              :: stud
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=InputFile, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + NAME_LEN + PATRONYMIC_LEN)*CH_
      open (file=DataFile, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(2(a, 1x), a)'
         do i = 1, LIST_LEN
            read (In, format, iostat=IO) stud
            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            
            write (Out, iostat=IO, rec=i) stud
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка: фамилии, имена и отчества.
   function Read_list(DataFile) result(List)
      type(student)             :: List(LIST_LEN)
      character(*), intent(in)  :: DataFile

      integer                   :: In, IO, recl
      
      recl = (SURNAME_LEN + LIST_LEN + PATRONYMIC_LEN)*CH_*LIST_LEN
      open (file=DataFile, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) List
         call Handle_IO_status(IO, "reading unformatted list")
      close (In)
   end function Read_list
 
   ! Вывод списка.
   subroutine Output_list(Output_File, List, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(in)  :: List(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         if (Position == 'append') write (Out, '(a)')
         write (Out, '(a)') List_name
         format = '(2(a, 1x), a)'
         write (Out, format, iostat=IO) List
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_list
end module List_IO
