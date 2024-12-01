pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day_1_2 is
   package Positive_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Positive);
   package Natural_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Natural);

   F : File_Type;
   Left_List, Right_List : Positive_Vectors.Vector;
   Similarity_Scores     : Natural_Vectors.Vector;
begin
   --  Open and read the file into the lists
   Open (F, In_File, "input.txt");
   while not End_Of_File (F) loop
      declare
         Line : constant String := Get_Line (F);
      begin
         --  Get the left number and put it into list
         Left_List.Append (Positive'Value (Line (1 .. 5)));
         --  Get the right number and put it into list
         Right_List.Append (Positive'Value (Line (9 .. 13)));
      end;
   end loop;
   Close (F);

   Put_Line ("Read " & Left_List.Length'Image & " lines");

   --  Get similarity scores from the lists
   for L of Left_List loop
      declare
         Appearance_Counter : Integer := 0;
      begin
         for R of Right_List loop
            if L = R then
               Appearance_Counter := @ + 1;
            end if;
         end loop;

         Similarity_Scores.Append (L * Appearance_Counter);
      end;
   end loop;

   --  Calculate total similarity score
   declare
      Total_Score : Natural := 0;
   begin
      for S of Similarity_Scores loop
         Total_Score := @ + S;
      end loop;
      Put_Line ("Total similarity score is: " & Total_Score'Image);
   end;

end Day_1_2;
