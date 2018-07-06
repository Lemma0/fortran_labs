program lab_1_1_15
   use Environment

   implicit none
   integer, parameter         :: LIST_LEN = 15, SURNAME_LEN = 15, NAME_LEN = 10, PATRONYMIC_LEN = 15
   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, имен и отчеств, а также временные переменные для обменов при сортировке.
   character(SURNAME_LEN, kind=CH_)                  :: Surnames(LIST_LEN) = ""
   character(SURNAME_LEN, kind=CH_),    allocatable  :: ProcessedSurnames(:)
   
   character(NAME_LEN, kind=CH_)                     :: Names(LIST_LEN) = ""
   character(NAME_LEN, kind=CH_),       allocatable  :: UniqueNames(:), ProcessedNames(:)
   
   character(PATRONYMIC_LEN, kind=CH_)               :: Patronymics(LIST_LEN) = ""
   character(PATRONYMIC_LEN, kind=CH_), allocatable  :: ProcessedPatronymics(:)

   logical, dimension(1:LIST_LEN)                    :: Mask = .true.
   logical                                           :: HasMet
   integer                                           :: In, Out, IO, i, j, k

   input_file = "data/group_list.txt"
   output_file = "output.txt"
   ! Чтение списка: фамилий, имен и отчеств.
   open (file=input_file, encoding=E_, newunit=In)
      format = '(a' // SURNAME_LEN // ', 1x, a' // NAME_LEN // ', 1x, a' // PATRONYMIC_LEN // ')'
      read (In, format, iostat=IO) (Surnames(i), Names(i), Patronymics(i), i = 1, LIST_LEN)
   close (In)

   ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading list."
      case(1:)
         write (Out, '(a)') "Error while reading list: ", IO
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", IO
   end select

   ! Вывод списка.
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Names(i), Patronymics(i), i = 1, LIST_LEN)
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", IO
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing list: ", IO
   end select

   ! Составление маски строк с уникальными именами.
   allocate(UniqueNames(LIST_LEN))
   ! Минимальное количество уникальных имен.
   k = 1
   ! Первое уникальное имя - из первой строки списка.
   UniqueNames(1) = Names(1)
   ! Опрашивание каждого последующего имени в списке.
   do i = 2, LIST_LEN
      ! Проверка, не присутствует ли имя из текущей строки в списке уникальных имен.
      HasMet = .false.
      do j = 1, k
        if (Names(i) == UniqueNames(j)) then
           HasMet = .true.
        end if
      end do
      ! Если оно встречалось, то записываем об этом информацию в маску.
      if (HasMet) then
         Mask(i) = .false.
      ! Если не встречалось, добавляем имя к списку уникальных имен.
      else
         k = k+1
         UniqueNames(k) = Names(i)
      end if
   end do

   ! Применение маски к архивам фамилий, имен, отчеств.
   ProcessedSurnames    = Pack(Surnames, Mask)
   ProcessedNames       = Pack(Names, Mask)
   ProcessedPatronymics = Pack(Patronymics, Mask)

   ! Вывод прореженного списка.
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (Out, '(/a)') "Прореженный список:"
      write (Out, format, iostat=IO) (ProcessedSurnames(i), ProcessedNames(i), &
         ProcessedPatronymics(i), i = 1, Size(ProcessedNames))
   close (Out)
   ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing diluted list."
      case(1:)
         write (Out, '(a)') "Error while writing diluted list: ", IO
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing diluted list: ", IO
   end select

end program lab_1_1_15
