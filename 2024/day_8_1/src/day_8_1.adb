pragma Ada_2022;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_8_1 is

   type Map_Point is new Character
     with Static_Predicate => Map_Point in
       '#' | '.' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9';

   subtype Frequency_ID is Map_Point
     with Static_Predicate => Frequency_ID in
       'a' .. 'z' | 'A' .. 'Z' | '0' .. '9';

   type X_Index is new Positive;
   type Y_Index is new Positive;

   type Coordinate is record
      X : X_Index;
      Y : Y_Index;
   end record;

   package Coordinate_Vectors is
     new Ada.Containers.Vectors (Positive, Coordinate);
   subtype Coordinate_List is Coordinate_Vectors.Vector;
   use type Coordinate_List;

   function Hash (Frequency : Frequency_ID) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Frequency_ID'Pos (Frequency)));

   package Frequency_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Frequency_ID,
      Element_Type    => Coordinate_List,
      Hash            => Hash,
      Equivalent_Keys => "=");
   subtype Frequency_Records is Frequency_Maps.Map;

   type Antenna_Map is array (Y_Index range <>, X_Index range <>)
     of Map_Point;

   function Load_Map (Name    : String;
                      Rows    : in out Y_Index;
                      Columns : in out X_Index) return Antenna_Map is
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

      Put_Line ("Getting map dimensions...");

      Get_File_Dimensions (Name, R, C);
      Rows := R;
      Columns := C;

      Put_Line ("Dimensions: " & Rows'Image & Columns'Image);

      return M : Antenna_Map (1 .. Rows, 1 .. Columns) do
         Open (F, In_File, Name);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               for Col in Line'Range loop
                  M (Row, X_Index (Col)) := Frequency_ID (Line (Col));
               end loop;
               Row := @ + 1;
            end;
         end loop;
         Close (F);
      end return;
   end Load_Map;

   function In_Bounds (M : Antenna_Map; P : Coordinate) return Boolean is
     (P.Y in M'Range (1) and then P.X in M'Range (2));

   Rows    : Y_Index := 1;
   Columns : X_Index := 1;
   Frequencies : Frequency_Records;
   M           : constant Antenna_Map := Load_Map ("input.txt", Rows, Columns);
   Antinodes   : Antenna_Map := M;
   Antinode_Count : Natural := 0;
begin
   for R in M'Range (1) loop
      for C in M'Range (2) loop
         declare
            Point : constant Map_Point := M (R, C);
         begin
            if Point in Frequency_ID then
               --  Add frequency
               if not Frequencies.Contains (Point) then
                  Frequencies.Include (Point, Coordinate_Vectors.Empty_Vector);
               end if;
               Frequencies (Point).Append
                 (Coordinate'(X_Index (C), Y_Index (R)));
            end if;
         end;
      end loop;
   end loop;

   for F of Frequencies loop
      --  Put (Frequency_Maps.Key (F)'Image);
      for Current in 1 .. F.Last_Index loop
         for Index in Current + 1 .. F.Last_Index loop
            --  Add each antinode for each pair of coordinates
            declare
               X_Delta : constant Integer :=  Integer (F (Index).X) -
                           Integer (F (Current).X);
               Y_Delta : constant Integer :=  Integer (F (Index).Y) -
                           Integer (F (Current).Y);
            begin
               Put (X_Delta'Image);
               Put (Y_Delta'Image);

               if F (Index).X - F (Current).X > 0
                 and then F (Index).Y - F (Current).Y > 0
               then
                  declare
                     Antinode_Pos : constant Coordinate :=
                                      (F (Index).X - F (Current).X,
                                       F (Index).Y - F (Current).Y);
                  begin
                     if In_Bounds (M, Antinode_Pos) then
                        Antinodes (Antinode_Pos.Y, Antinode_Pos.X) := '#';
                        Antinode_Count := @ + 1;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;
   end loop;

   Put_Line ("Antinode count: " & Antinode_Count'Image);

end Day_8_1;
