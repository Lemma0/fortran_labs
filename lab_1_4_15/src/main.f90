program lab_1_3_15
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable     :: InputFile, OutputFile, DataFile
   
   type(student)                 :: List(LIST_LEN)
   type(student), allocatable    :: DilutedList(:)
   logical, dimension(LIST_LEN)  :: Mask

   InputFile  = "data/group_list.txt"
   OutputFile = "output.txt"
   DataFile   = "list.dat"
   
   call Create_data_file(InputFile, DataFile)
   
   List = Read_list(DataFile)

   call Output_list(OutputFile, List, 'Исходный список:', 'rewind')

   Mask = Create_Mask(List)
   DilutedList = pack(List, Mask)

   call Output_list(OutputFile, DilutedList, 'Прореженный список:', 'append')

end program lab_1_3_15
