pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_4_1 is

   procedure Get_File_Dimensions (Name    : String;
                                  Columns : out Natural;
                                  Lines   : out Natural) is
      F          : File_Type;
      Line_Count : Natural := 0;
   begin
      Open (F, In_File, Name);

      if not End_Of_File (F) then
         declare
            Line : constant String := Get_Line (F);
         begin
            Columns := Line'Last;
            Line_Count := 1;
         end;
      end if;

      while not End_Of_File (F) loop
         Skip_Line (F);
         Line_Count := @ + 1;
      end loop;

      Lines := Line_Count;

      Close (F);
   end Get_File_Dimensions;

   type Map is array (Positive range <>, Positive range <>) of Character;

   Columns : Natural := 0;
   Lines : Natural := 0;
   F : File_Type;
begin
   Get_File_Dimensions ("input.txt", Columns, Lines);

   declare
      M          : Map (1 .. Lines, 1 .. Columns);
      Line_Index : Natural := 1;
   begin
      Open (F, In_File, "input.txt");
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            for X in Line'Range loop
               M (Line_Index, X) := Line (X);
            end loop;
            Line_Index := @ + 1;
         end;
      end loop;

      for Y in M'Range (1) loop
         for X in M'Range (2) loop
            Put (M (Y,X));
         end loop;
         Put_Line ("");
      end loop;

   end;

   Put_Line ("Dimensions: " & Columns'Image & ", " & Lines'Image);
end Day_4_1;
