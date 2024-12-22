pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_9_2 is

   type FileID is new Integer range -1 .. 19999;

   type Disk_File is record
      Id    : FileID;
      Start : Natural;
      Blocks : Positive;
   end record;

   package Disk_Block_Vectors is
     new Ada.Containers.Vectors (Natural, FileID);

   package Disk_File_Vectors is
     new Ada.Containers.Vectors (Natural, Disk_File);

   procedure Generate_Disk (Files  : in out Disk_File_Vectors.Vector;
                            Blocks : in out Disk_Block_Vectors.Vector;
                            Name   : String) is
      F : File_Type;
      S : String (1 .. 1);
      File_Length  : Positive;
      Free_Length  : Natural;
      Next_File_ID : FileID := 0;
      Last_Index : Natural := 0;
   begin
      Open (F, In_File, Name);

      while not End_Of_File (F) loop
         --  Get file length digit
         Get (F, S);
         File_Length := Positive'Value (S);

         Files.Append (Disk_File'(
                       Next_File_ID,
                       Last_Index,
                       File_Length));

         for I in 1 .. File_Length loop
            Blocks.Append (Next_File_ID);
            Last_Index := @ + 1;
         end loop;

         Next_File_ID := @ + 1;

         --  Get free space digit
         if not End_Of_File (F) then
            Get (F, S);
            Free_Length := Natural'Value (S);
            if Free_Length /= 0 then
               for I in 1 .. Free_Length loop
                  Blocks.Append (-1);
                  Last_Index := @ + 1;
               end loop;
            end if;
         end if;
      end loop;

      Close (F);
   end Generate_Disk;

   function Get_Next_Free_Space (Disk      : Disk_Block_Vectors.Vector;
                                 Right_Idx : Natural;
                                 Size      : Natural) return Integer is
      Count : Natural := 0;
      Found_Space : Boolean := False;
      First_Block : Integer := -1;
   begin
      for I in Disk.First_Index .. Right_Idx loop
         if Disk.Element (I) = -1 then
            if Found_Space = False then
               Found_Space := True;
               First_Block := I;
            end if;
            Count := @ + 1;
         else
            if Found_Space then
               if Count >= Size then
                  exit;
               else
                  --  Not enough space, reset the count and start again
                  Found_Space := False;
                  First_Block := -1;
                  Count := 0;
               end if;
            end if;
         end if;
      end loop;

      return First_Block;

   end Get_Next_Free_Space;

   Files : Disk_File_Vectors.Vector := Disk_File_Vectors.Empty_Vector;
   Blocks : Disk_Block_Vectors.Vector := Disk_Block_Vectors.Empty_Vector;

   Checksum : Long_Long_Integer := 0;
   Next_Space : Integer := 0;
begin
   --  Generate disk from map
   Generate_Disk (Files, Blocks, "../day_9_1/input.txt");

   --  "Defrag" the disk
   --  Loop on files in reverse
   for I in reverse Files.First_Index + 1 .. Files.Last_Index loop
      --  Find if there's a space to the left where the file fits
      Next_Space := Get_Next_Free_Space (Blocks,
                                         Files.Element (I).Start,
                                         Files.Element (I).Blocks);
      if Next_Space /= -1 then
         for J in 0 .. Files.Element (I).Blocks - 1 loop
            Blocks (Next_Space + J) := Blocks (Files.Element (I).Start + J);
            Blocks (Files.Element (I).Start + J) := -1;
         end loop;
      end if;
   end loop;

   --  Calculate checksum
   for I in Blocks.First_Index .. Blocks.Last_Index loop
      if Blocks.Element (I) /= -1 then
         Checksum := @ +
           Long_Long_Integer (I) * Long_Long_Integer (Blocks.Element (I));
      end if;
   end loop;

   Put_Line ("Checksum: " & Checksum'Image);
end Day_9_2;
