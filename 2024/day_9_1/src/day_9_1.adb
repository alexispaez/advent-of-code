pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_9_1 is

   type FileID is new Integer range -1 .. 19999;

   package Disk_Block_Vectors is
     new Ada.Containers.Vectors (Natural, FileID);

   procedure Generate_Disk (Disk : in out Disk_Block_Vectors.Vector;
                            Name : String) is
      F : File_Type;
      S : String (1 .. 1);
      File_Length  : Positive;
      Free_Length  : Natural;
      Next_File_ID : FileID := 0;
   begin
      Open (F, In_File, Name);

      while not End_Of_File (F) loop
         --  Get file length digit
         Get (F, S);
         File_Length := Positive'Value (S);
         for I in 1 .. File_Length loop
            Disk.Append (Next_File_ID);
         end loop;

         Next_File_ID := @ + 1;

         --  Get free space digit
         if not End_Of_File (F) then
            Get (F, S);
            Free_Length := Natural'Value (S);
            if Free_Length /= 0 then
               for I in 1 .. Free_Length loop
                  Disk.Append (-1);
               end loop;
            end if;
         end if;
      end loop;

      Close (F);
   end Generate_Disk;

   procedure Display_Disk (Disk : Disk_Block_Vectors.Vector) is
   begin
      for I in Disk.First_Index .. Disk.Last_Index loop
         if Disk.Element (I) = -1 then
            Put ('.');
         else
            Put (Trim (Disk.Element (I)'Image, Both));
         end if;
      end loop;
      New_Line;

   end Display_Disk;

   procedure Get_Last_Used_Block (Disk      : Disk_Block_Vectors.Vector;
                                  Left_Idx  : Natural;
                                  Right_Idx : in out Natural) is
   begin
      for I in reverse Left_Idx .. Right_Idx loop
         if Disk.Element (I) /= -1 then
            Right_Idx := I;
            return;
         end if;
      end loop;
   end Get_Last_Used_Block;

   procedure Get_Next_Free_Block (Disk     : Disk_Block_Vectors.Vector;
                                 Left_Idx  : in out Natural;
                                 Right_Idx : Natural) is
   begin
      for I in Left_Idx .. Right_Idx loop
         if Disk.Element (I) = -1 then
            Left_Idx := I;
            return;
         end if;
      end loop;
   end Get_Next_Free_Block;

   procedure Swap (Disk  : in out Disk_Block_Vectors.Vector;
                   Left  : Natural;
                   Right : Natural) is
      File_Id : FileID := 0;
   begin
      File_Id := Disk (Left);
      Disk (Left) := Disk (Right);
      Disk (Right) := File_Id;
   end Swap;

   Disk : Disk_Block_Vectors.Vector := Disk_Block_Vectors.Empty_Vector;

   Left_Index  : Natural;
   Right_Index : Natural;
   Checksum : Long_Long_Integer := 0;
begin
   --  Generate disk from map
   Generate_Disk (Disk, "input.txt");

   --  Display_Disk (Disk);

   --  "Defrag" the disk
   Left_Index := Disk.First_Index;
   Right_Index := Disk.Last_Index;

   loop
      Get_Last_Used_Block (Disk, Left_Index, Right_Index);
      Get_Next_Free_Block (Disk, Left_Index, Disk.Last_Index);

      exit when Right_Index <= Left_Index;

      Swap (Disk, Left_Index, Right_Index);

      --  Display_Disk (Disk);
   end loop;

   --  Calculate checksum
   for I in Disk.First_Index .. Disk.Last_Index loop
      if Disk.Element (I) /= -1 then
         Checksum := @ +
           Long_Long_Integer (I) * Long_Long_Integer (Disk.Element (I));
      end if;
   end loop;

   Put_Line ("Checksum: " & Checksum'Image);
end Day_9_1;
