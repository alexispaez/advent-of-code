pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_6_1 is

   type Map_Type is array (Positive range <>, Positive range <>) of Character;

   type Direction_Type is (North, East, South, West);

   type Coordinate_Type is record
      Column, Row : Integer;
   end record;

   Direction_Offsets : constant array (Direction_Type'Range)
     of Coordinate_Type := [(0, -1), (1, 0), (0, 1), (-1, 0)];

   function Is_Inbounds (M : Map_Type; P : Coordinate_Type) return Boolean is
     (P.Column in M'Range (1) and then P.Row in M'Range (2));

   function Get_Next_Position (Pos : Coordinate_Type;
                               Dir : Direction_Type) return Coordinate_Type is
   begin
      return C : Coordinate_Type do
         C.Column := Pos.Column + Direction_Offsets (Dir).Column;
         C.Row := Pos.Row + Direction_Offsets (Dir).Row;
      end return;
   end Get_Next_Position;

   function Load_Map (Name    : String;
                      Rows    : in out Natural;
                      Columns : in out Natural;
                      Start   : in out Coordinate_Type) return Map_Type is
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

      return M : Map_Type (1 .. Rows, 1 .. Columns) do
         Open (F, In_File, Name);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               for Col in Line'Range loop
                  M (Row, Col) := Line (Col);
                  if M (Row, Col) = '^' then
                     Start.Column := Col;
                     Start.Row := Row;
                     --  Replace the starting mark with a mark
                     M (Row, Col) := 'X';
                  end if;
               end loop;
               Row := @ + 1;
            end;
         end loop;
         Close (F);
      end return;
   end Load_Map;

   Rows        : Natural := 0;
   Columns     : Natural := 0;
   Current_Pos : Coordinate_Type := (0, 0);
   Map         : Map_Type :=
                   Load_Map ("input.txt", Rows, Columns, Current_Pos);
   Next_Pos    : Coordinate_Type := (0, 0);
   Direction   : Direction_Type := North;
   Visited     : Natural := 1;

   --  Debug_Count : Natural := 0;
begin
   --  Put_Line ("Start position: " &
   --              Current_Pos.Row'Image &
   --              Current_Pos.Column'Image);
   loop
      Next_Pos := Get_Next_Position (Current_Pos, Direction);

      --  Put_Line ("Next position: " &
      --              Next_Pos.Row'Image &
      --              Next_Pos.Column'Image);

      if not Is_Inbounds (Map, Next_Pos) then
         --  Put_Line ("Out of bounds at: " & Next_Pos'Image);
         exit;
      else
         declare
            Char : constant Character := Map (Next_Pos.Row, Next_Pos.Column);
         begin
            case Char is
            when '#' =>
               --  Turn right
               if Direction = Direction_Type'Last then
                  Direction := Direction_Type'First;
               else
                  Direction := Direction_Type'Succ (Direction);
               end if;
               --  Put ("Found #, turn " & Direction'Image);
            when '.' | 'X' =>
               --  Continue ahead
               if Char = '.' then
                  Visited := @ + 1;
                  Map (Next_Pos.Row, Next_Pos.Column) := 'X';
               end if;
               Current_Pos := Next_Pos;
               when others => null;
            end case;
         end;
      end if;

      --  Debug_Count := @ + 1;
      --  exit when Debug_Count = 100;
   end loop;

   Put_Line ("Visited positions: " & Visited'Image);

   for R in 1 .. Rows loop
      for C in 1 .. Columns loop
         Put (Map (R, C));
      end loop;
      New_Line;
   end loop;
end Day_6_1;
