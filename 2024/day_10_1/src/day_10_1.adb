pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_10_1 is

   type Path_Height is new Natural range 0 .. 9;

   type X_Index is new Integer;
   type Y_Index is new Integer;

   type Topographical_Map is array (Y_Index range <>, X_Index range <>)
     of Path_Height;

   type Visited_Map is array (Y_Index range <>, X_Index range <>)
     of Boolean;

   type Coordinate_Type is record
      Column : X_Index;
      Row    : Y_Index;
   end record;

   type Direction_Type is (Up, Right, Down, Left);

   Direction_Offsets  : constant array (Direction_Type'Range)
     of Coordinate_Type := [ (0, -1), (1, 0), (0, 1), (-1, 0)];

   function Load_Map (Name    : String;
                      Rows    : in out Y_Index;
                      Columns : in out X_Index) return Topographical_Map is
      procedure Get_File_Dimensions (Name    : String;
                                     Rows    : out Y_Index;
                                     Columns : out X_Index) is
         F          : File_Type;
         Row_Count  : Y_Index;
      begin
         Open (F, In_File, Name);

         if not End_Of_File (F) then
            declare
               Line : constant String := Get_Line (F);
            begin
               Columns := X_Index (Line'Last);
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

      R   : Y_Index;
      C   : X_Index;
      Row : Y_Index := 1;
      F   : File_Type;
   begin

      Get_File_Dimensions (Name, R, C);
      Rows := R;
      Columns := C;

      return M : Topographical_Map (1 .. Rows, 1 .. Columns) do
         Open (F, In_File, Name);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               for Col in Line'Range loop
                  M (Row, X_Index (Col)) :=
                    Path_Height'Value (Line (Col .. Col));
               end loop;
               Row := @ + 1;
            end;
         end loop;
         Close (F);
      end return;
   end Load_Map;

   function Is_Inbounds (M : Topographical_Map;
                         P : Coordinate_Type) return Boolean is
     (P.Column in M'Range (2) and then P.Row in M'Range (1));

   function Get_Next_Position (Pos : Coordinate_Type;
                               Dir : Direction_Type) return Coordinate_Type is
   begin
      return C : Coordinate_Type do
         C.Column := Pos.Column + Direction_Offsets (Dir).Column;
         C.Row := Pos.Row + Direction_Offsets (Dir).Row;
      end return;
   end Get_Next_Position;

   procedure Hike (M      : Topographical_Map;
                   V      : in out Visited_Map;
                   Pos    : Coordinate_Type;
                   Level  : Path_Height;
                   Score  : in out Natural;
                   Rating : in out Natural) is
   begin

      --  Return if the position is not within the map bounds
      if not Is_Inbounds (M, Pos) then
         return;
      end if;

      case Level is
      when 0 =>
         --  For the initial level, no need to do anything
         null;
      when 1 .. 8 =>
         --  Check if the gradient is correct
         if M (Pos.Row, Pos.Column) - Level /= 0
         then
            return;
         end if;
      when 9 =>
         --  Check if found the end of the trail
         if M (Pos.Row, Pos.Column) = 9 then
            --  Count for part 2
            Rating := @ + 1;
            if V (Pos.Row, Pos.Column) = False then
               --  For part 1
               Score := @ + 1;
               V (Pos.Row, Pos.Column) := True;
            end if;
            return;
         else
            return;
         end if;
      end case;

      Hike (M, V, Get_Next_Position (Pos, Up), Level + 1, Score, Rating);
      Hike (M, V, Get_Next_Position (Pos, Right), Level + 1, Score, Rating);
      Hike (M, V, Get_Next_Position (Pos, Down), Level + 1, Score, Rating);
      Hike (M, V, Get_Next_Position (Pos, Left), Level + 1, Score, Rating);
   exception
      when others =>
         Put_Line ("Exception: " &
                     Pos.Row'Image &
                     Pos.Column'Image &
                     Level'Image);
   end Hike;

   Rows    : Y_Index := 1;
   Columns : X_Index := 1;
   M       : constant Topographical_Map := Load_Map
     ("input.txt", Rows, Columns);
   Score   : Natural := 0;
   Rating  : Natural := 0;
   Visited : Visited_Map (M'Range (1), M'Range (2)) :=
     [others => [others => False]];
begin
   --  Find all trail heads
   Outer_Loop :
   for Y in M'Range (1) loop
      for X in M'Range (2) loop
         if M (Y, X) = 0 then
            Hike (M, Visited, Coordinate_Type'(X, Y), 0, Score, Rating);
            Visited := [others => [others => False]];
         end if;
      end loop;
   end loop Outer_Loop;

   Put_Line ("Total score: " & Score'Image);
   Put_Line ("Total rating: " & Rating'Image);
end Day_10_1;
