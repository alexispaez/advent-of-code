pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_4_1 is

   type Map is array (Positive range <>, Positive range <>) of Character;

   type Direction is (North, North_East, East, South_East,
                      South, South_West, West, North_West);

   type Coordinates is record
      Column, Row : Integer;
   end record;

   Direction_Offsets : constant array (Direction'Range)
     of Coordinates := [ (0, -1), (1, -1), (1, 0), (1, 1),
                         (0, 1), (-1, 1), (-1, 0), (-1, -1)];

   function Is_Inbounds (M : Map; P : Coordinates) return Boolean is
     (P.Column in M'Range (1) and then P.Row in M'Range (2));

   function Get_Next_Position (Pos : Coordinates;
                               Dir : Direction) return Coordinates is
   begin
      return C : Coordinates do
         C.Column := Pos.Column + Direction_Offsets (Dir).Column;
         C.Row := Pos.Row + Direction_Offsets (Dir).Row;
      end return;
   end Get_Next_Position;

   function Load_Map (Name    : String;
                      Rows    : in out Natural;
                      Columns : in out Natural) return Map is
      procedure Get_File_Dimensions (Name    : String;
                                     Rows    : out Natural;
                                     Columns : out Natural) is
         F          : File_Type;
         Row_Count  : Natural := 0;
      begin
         Open (F, In_File, Name);

         if not End_Of_File (F) then
            declare
               Line : constant String := Get_Line (F);
            begin
               Columns := Line'Last;
               Row_Count := 1;
            end;
         end if;

         while not End_Of_File (F) loop
            Skip_Line (F);
            Row_Count := @ + 1;
         end loop;

         Rows := Row_Count;

         Close (F);
      end Get_File_Dimensions;

      R   : Natural := 0;
      C   : Natural := 0;
      F   : File_Type;
      Row : Natural := 1;
   begin
      Get_File_Dimensions (Name, R, C);
      Rows := R;
      Columns := C;

      return M : Map (1 .. Rows, 1 .. Columns) do
         Open (F, In_File, Name);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               for Col in Line'Range loop
                  M (Row, Col) := Line (Col);
               end loop;
               Row := @ + 1;
            end;
         end loop;
         Close (F);
      end return;
   end Load_Map;

   function Find_String (M             : Map;
                         Column        : Natural;
                         Row           : Natural;
                         Search_String : String) return Natural is
      S         : String (Search_String'Range);
      Count     : Natural := 0;
      Start_Pos : Coordinates;
      Valid     : Boolean;
   begin
      --  Get characters in all 8 directions
      for D in Direction'Range loop
         --  Reset to initial character
         Start_Pos := (Column, Row);
         Valid := True;
         for I in S'Range loop
            Start_Pos := Get_Next_Position (Start_Pos, D);

            if Is_Inbounds (M, Start_Pos) then
               S (I) := M (Start_Pos.Row, Start_Pos.Column);
            else
               Valid := False;
               exit;
            end if;
         end loop;

         if Valid then
            if S = Search_String then
               Count := @ + 1;
            end if;
         end if;
      end loop;
      return Count;
   end Find_String;

   function Find_X_MAS (M      : Map;
                        Column : Natural;
                        Row    : Natural) return Natural is
      S1        : String (1 .. 3);
      S2        : String (1 .. 3);
      Count     : Natural := 0;
      Start_Pos : constant Coordinates := (Column, Row);
      Valid     : Boolean := True;
   begin
      --  Check if position is valid for an X-MAS
      if Is_Inbounds (M, Get_Next_Position (Start_Pos, North_West)) and then
        Is_Inbounds (M, Get_Next_Position (Start_Pos, South_East)) and then
        Is_Inbounds (M, Get_Next_Position (Start_Pos, North_East)) and then
        Is_Inbounds (M, Get_Next_Position (Start_Pos, South_West))
      then
         --  Get the strings spanning the X
         S1 (1) := M (Get_Next_Position (Start_Pos, North_West).Row,
                      Get_Next_Position (Start_Pos, North_West).Column);
         S1 (2) := 'A';
         S1 (3) := M (Get_Next_Position (Start_Pos, South_East).Row,
                      Get_Next_Position (Start_Pos, South_East).Column);

         S2 (1) := M (Get_Next_Position (Start_Pos, North_East).Row,
                      Get_Next_Position (Start_Pos, North_East).Column);
         S2 (2) := 'A';
         S2 (3) := M (Get_Next_Position (Start_Pos, South_West).Row,
                      Get_Next_Position (Start_Pos, South_West).Column);
      else
         Valid := False;
      end if;

      if Valid then
         if (S1 = "MAS" or else S1 = "SAM")
           and then (S2 = "MAS" or else S2 = "SAM")
         then
            Count := @ + 1;
         end if;
      end if;
      return Count;
   end Find_X_MAS;

   Rows    : Natural := 0;
   Columns : Natural := 0;
   M           : constant Map := Load_Map ("input.txt", Rows, Columns);
   Count_XMAS  : Natural := 0;
   Count_X_MAS : Natural := 0;
begin
   for R in 1 .. Rows loop
      for C in 1 .. Columns loop
         --  Find XMAS strings
         if M (R, C) = 'X' then
            Count_XMAS := @ + Find_String (M, C, R, "MAS");
         end if;
         --  Find X-MAS exes
         if M (R, C) = 'A' then
            Count_X_MAS := @ + Find_X_MAS (M, C, R);
         end if;
      end loop;
   end loop;

   Put_Line ("Found: " & Count_XMAS'Image & " XMAS strings.");
   Put_Line ("Found: " & Count_X_MAS'Image & " X_MAS strings.");
end Day_4_1;
