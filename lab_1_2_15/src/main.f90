program lab_1_2_15
   use Environment

   implicit none
   integer, parameter            :: LIST_LEN = 15, SURNAME_LEN = 15, NAME_LEN = 10, PATRONYMIC_LEN = 15
   character(:), allocatable     :: input_file, output_file

   ! Массивы фамилий, имен и отчеств.
   character(kind=CH_)           ::    Surnames(LIST_LEN, SURNAME_LEN)    = "", &
                                          Names(LIST_LEN, NAME_LEN)       = "", &
                                    Patronymics(LIST_LEN, PATRONYMIC_LEN) = ""

   character(kind=CH_), allocatable, dimension(:, :)  :: DilutedSurnames(:, :), &
                                                         DilutedNames(:, :), &
                                                         DilutedPatronymics(:, :)

   logical, dimension(LIST_LEN)  :: MaskLine

   input_file  = "data/group_list.txt"
   output_file = "diluted_group_list.txt"
   
   call Read_list(input_file, Surnames, Names, Patronymics)
   call Output_list(output_file, Surnames, Names, Patronymics, "Исходный список:", 'rewind')

   ! Поиск уникальных имен и выделение строк с ними в списке
   MaskLine = Produce_Mask_Line(Names)

   ! Прореживание списков фамилий, имен и отчеств по полученной маске
   DilutedSurnames    = Dilute(Surnames, SURNAME_LEN, MaskLine)
   DilutedNames       = Dilute(Names, NAME_LEN, MaskLine)
   DilutedPatronymics = Dilute(Patronymics, PATRONYMIC_LEN, MaskLine)

   call Output_list(output_file, DilutedSurnames, DilutedNames, DilutedPatronymics, &
      "Прореженный список:", 'append')

contains
   ! Чтение списка: фамилий, имен и отчеств.
   subroutine Read_list(Input_File, Surnames, Names, Patronymics)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Names(:, :), Patronymics(:, :)
      intent (in)          Input_File
      intent (out)         Surnames, Names, Patronymics

      integer                    :: In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // NAME_LEN // 'a1, 1x, ' // PATRONYMIC_LEN // 'a1)'
         read (In, format, iostat=IO) (Surnames(i, :), Names(i, :), Patronymics(i, :), i = 1, LIST_LEN)
         call Handle_IO_status(IO, "reading list")
      close (In)
   end subroutine Read_list

   ! Функция выделяет из списка строки с уникальными именами и создает по ним маску
   pure function Produce_Mask_Line(Names) result(MaskLine)
      logical, dimension(LIST_LEN)                       :: MaskLine(LIST_LEN)
      character(kind=CH_), intent(in)                    :: Names(LIST_LEN, NAME_LEN)

      character(kind=CH_), allocatable, dimension(:, :)  :: UniqueNames(:, :)
      integer                                            :: i, j, k, l
      logical                                            :: HasMet

      MaskLine = .true.
      ! Составление списка строк с уникальными именами.
      allocate(UniqueNames(LIST_LEN, NAME_LEN))
      ! Минимальное количество уникальных имен.
      k = 1
      ! Первое уникальное имя - из первой строки списка.
      do l = 1, NAME_LEN
         UniqueNames(1, l) = Names(1, l)
         !write(6,'(a)') UniqueNames(1, l)
      end do
      ! Опрашивание каждого последующего имени в списке.
      do i = 2, LIST_LEN
         ! Проверка, не присутствует ли имя из текущей строки в списке уникальных имен.
         HasMet = .false.
         do j = 1, k
            ! Если все имя целиком совпадает, значит оно встречалось.
            if (all(Names(i,:) == UniqueNames(j,:))) HasMet = .true.
         end do
         ! Если оно встречалось, то записываем об этом информацию в маску.
         if (HasMet) then
            MaskLine(i) = .false.
         ! Если не встречалось, добавляем имя к списку уникальных имен.
         else
            k = k + 1
            do l = 1, NAME_LEN
               UniqueNames(k,l) = Names(i,l)
            end do
         end if
      end do
   end function Produce_Mask_Line

   pure function Dilute(Attributes, Attribute_Len, MaskLine) result(DilutedAttributes)
      character(kind=CH_), allocatable, dimension(:, :)  :: DilutedAttributes(:, :)
      integer,                          intent(in)       :: Attribute_Len
      character(kind=CH_),              intent(in)       :: Attributes(LIST_LEN, Attribute_Len)
      logical, dimension(LIST_LEN),     intent(in)       :: MaskLine

      character(kind=CH_), allocatable, dimension(:)     :: ProcessedAttributes(:)
      logical, allocatable, dimension(:,:)               :: AttributeMask(:, :)
      integer :: i, l, pa

      ! Составление индивидуальных масок для каждого аттрибута строки.
      allocate(AttributeMask(LIST_LEN, Attribute_Len))
      do i = 1, LIST_LEN
         AttributeMask(i, :) = MaskLine(i)
      end do
      ! Выработка прореженного списка аттрибута в виде одномерного массива.
      ProcessedAttributes = pack(Attributes, AttributeMask)
      ! Количество строк с уникальными именами.
      pa = size(ProcessedAttributes) / Attribute_Len
      ! Перевод прореженного списка аттрибута из одномерного массива в двумерный.
      allocate(DilutedAttributes(pa, Attribute_Len))
      do i = 1, pa
         do l = 1, Attribute_Len
            DilutedAttributes(i, l) = ProcessedAttributes((l-1)*pa + i)
         end do
      end do
   end function Dilute

   ! Вывод списка.
   subroutine Output_list(Output_File, Surnames, Names, Patronymics, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Names(:, :), Patronymics(:, :)
      intent (in)          Output_File, Surnames, Names, Patronymics, List_name, Position

      integer                    :: Out, IO, i
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         if (Position == 'append') write (Out, '(a)')
         write (Out, '(a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // NAME_LEN // 'a1, 1x, ' // PATRONYMIC_LEN // 'a1)'
         write (Out, format, iostat=IO) (Surnames(i, :), Names(i, :), Patronymics(i, :), i = 1, Size(Names)/NAME_LEN)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_list

end program lab_1_2_15
