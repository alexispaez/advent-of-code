pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day_1_1 is
   package Positive_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => Positive);
   package Positive_Vectors_Sorting is
      new Positive_Vectors.Generic_Sorting;

   F : File_Type;
   Left_List, Right_List : Positive_Vectors.Vector;
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

   --  Output the lists to check
   for C in Left_List.Iterate loop
      Put (Positive'Image (Left_List (Positive_Vectors.To_Index (C))));
      Put_Line (Positive'Image (Right_List (Positive_Vectors.To_Index (C))));
   end loop;

   --  Sort the lists
   Put_Line ("Sorting the lists...");
   Positive_Vectors_Sorting.Sort (Left_List);
   Positive_Vectors_Sorting.Sort (Right_List);

   --  Output the lists to check
   for C in Left_List.Iterate loop
      Put (Positive'Image (Left_List (Positive_Vectors.To_Index (C))));
      Put_Line (Positive'Image (Right_List (Positive_Vectors.To_Index (C))));
   end loop;

   --  Calculate the distances
   declare
      Total_Distance : Natural := 0;
   begin
      for C in Left_List.Iterate loop
         declare
            Item_Distance : constant Integer
              := abs (Left_List (Positive_Vectors.To_Index (C))
                      - Right_List (Positive_Vectors.To_Index (C)));
         begin
            Total_Distance := @ + Item_Distance;
         end;
      end loop;
      Put_Line ("Total distance is: " & Total_Distance'Image);
   end;

end Day_1_1;
