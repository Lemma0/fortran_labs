! Программа удаления из списка группы людей с совпадающими именами,
! кроме первого. Во входном файле F1 находится список группы, а в
! файле F2 — тот же список, а также его измененная версия с удаленными
! из него строками людей с совпадающими именами.

program lab_1_5_15
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:),   allocatable  :: F1, F2, InitialAccess, DilutedAccess, InitialMessage, DilutedMessage

   type(ListLine), pointer      :: InitialList  => Null()   ! Список группы.
   type(ListLine), pointer      :: DilutedList  => Null()   ! Прореженный список группы.

   F1 = "data/group_list.txt"
   F2 = "diluted_group_list.txt"
   InitialMessage = 'Начальный список:'
   DilutedMessage = 'Прореженный список:'
   InitialAccess  = 'sequential'
   DilutedAccess  = 'append'
   
   InitialList => Read_List(F1)

   if (Associated(InitialList)) then
      call Output_List(F2, InitialList, InitialMessage, InitialAccess)
      ! Следующая функция вызывается только с целью сохранить содержимое
      ! исходного списка.
      DilutedList => Dilute_List(InitialList)
      ! Если в сохранении исходного списка нет необходимости, использовать
      ! вместо вышеуказанной строки следующие две строки:
      !call Dilute_Copy(InitialList)
      !DilutedList => InitialList
      if (Associated(DilutedList)) &
         call Output_List(F2, DilutedList, DilutedMessage, DilutedAccess)
   end if
end program lab_1_5_15
