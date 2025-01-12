pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_12_2 is

   subtype Plot is Character range 'A' .. 'Z';

   type X_Index is new Integer;
   type Y_Index is new Integer;

   type Garden_Map is array (Y_Index range <>, X_Index range <>)
     of Plot;

   type Assigned_Map is array (Y_Index range <>, X_Index range <>)
     of Boolean;

   type Coordinate_Type is record
      Row    : Y_Index;
      Column : X_Index;
   end record;

   type Direction_Type is (Above, Right, Below, Left);

   Direction_Offsets  : constant array (Direction_Type'Range)
     of Coordinate_Type := [ (-1, 0), (0, 1), (1, 0), (0, -1)];

   package Region_Vectors is
     new Ada.Containers.Vectors (Natural, Coordinate_Type);
   subtype Plot_Vector_Type is Region_Vectors.Vector;
   use type Plot_Vector_Type;

   package Regions_Vectors is
     new Ada.Containers.Vectors (Natural, Plot_Vector_Type);
   subtype Region_Vector_Type is Regions_Vectors.Vector;

   function Get_Next_Position (Pos : Coordinate_Type;
                               Dir : Direction_Type) return Coordinate_Type is
   begin
      return C : Coordinate_Type do
         C.Column := Pos.Column + Direction_Offsets (Dir).Column;
         C.Row := Pos.Row + Direction_Offsets (Dir).Row;
      end return;
   end Get_Next_Position;

   function Load_Map (Name    : String;
                      Rows    : in out Y_Index;
                      Columns : in out X_Index) return Garden_Map is
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

      return M : Garden_Map (1 .. Rows, 1 .. Columns) do
         Open (F, In_File, Name);
         while not End_Of_File (F) loop
            declare
               Line : constant String := Get_Line (F);
            begin
               for Col in Line'Range loop
                  M (Row, X_Index (Col)) := Plot (Line (Col));
               end loop;
               Row := @ + 1;
            end;
         end loop;
         Close (F);
      end return;
   end Load_Map;

   function Is_Inbounds (M : Garden_Map;
                         P : Coordinate_Type) return Boolean is
     (P.Column in M'Range (2) and then P.Row in M'Range (1));

   procedure Find_Region (M      : Garden_Map;
                          A      : in out Assigned_Map;
                          Pos    : Coordinate_Type;
                          P      : Plot;
                          Region : in out Plot_Vector_Type) is
   begin
      if Is_Inbounds (M, Pos) and then
        M (Pos.Row, Pos.Column) = P and then
        A (Pos.Row, Pos.Column) = False
      then
         A (Pos.Row, Pos.Column) := True;
         Region.Append (Pos);

         Find_Region (M, A, Get_Next_Position (Pos, Above), P, Region);
         Find_Region (M, A, Get_Next_Position (Pos, Right), P, Region);
         Find_Region (M, A, Get_Next_Position (Pos, Below), P, Region);
         Find_Region (M, A, Get_Next_Position (Pos, Left), P, Region);
      end if;
   end Find_Region;

   function Get_Previous_Direction (Curr : Direction_Type)
                                    return Direction_Type is
   begin
      if Curr = Direction_Type'First then
         return Direction_Type'Last;
      else
         return Direction_Type'Pred (Curr);
      end if;
   end Get_Previous_Direction;

   function Get_Sides (M : Garden_Map;
                       R : Plot_Vector_Type) return Natural is
      Sides : Natural := 0;
   begin
      Put_Line ("get sides");
      for P in R.First_Index .. R.Last_Index loop
         for D in Direction_Type'Range loop
            declare
               Next_Pos : constant Coordinate_Type :=
                            Get_Next_Position (R (P), D);
               Prev_Pos : constant Coordinate_Type :=
                            Get_Next_Position (R (P),
                                               Get_Previous_Direction (D));
            begin
               if not Is_Inbounds (M, Next_Pos) or else
                 (Is_Inbounds (M, Next_Pos) and then
                  M (Next_Pos.Row, Next_Pos.Column) /=
                      M (R (P).Row, R (P).Column))
               then
                  --  There is a border, so check previous plot
                  --  to decide if it needs to be counted
                  if not Is_Inbounds (M, Prev_Pos) or else
                    (Is_Inbounds (M, Prev_Pos) and then
                    M (Prev_Pos.Row, Prev_Pos.Column) /=
                    M (R (P).Row, R (P).Column))
                  then
                     Sides := @ + 1;
                     Put ("Pos: " & Coordinate_Type'Image (R (P)));
                     Put_Line ("Sides: " & Sides'Image);
                     --  Put (" Dir: " & D'Image);
                     --  Put (" Nex: " & Next_Pos'Image);
                     --  Put (" Prv: " & Prev_Pos'Image);
                     New_Line;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      return Sides;
   end Get_Sides;

   --  Variables for the main procedure
   Rows    : Y_Index := 1;
   Columns : X_Index := 1;
   M       : constant Garden_Map := Load_Map
     ("../Day_12_1/input_small.txt", Rows, Columns);
   A       : Assigned_Map (M'Range (1), M'Range (2)) :=
               [others => [others => False]];
   Regions : Region_Vector_Type := Regions_Vectors.Empty_Vector;
   Price   : Natural := 0;
begin
   --  Find all regions
   Outer_Loop :
   for Y in M'Range (1) loop
      for X in M'Range (2) loop
         if A (Y, X) = False then
            declare
               Region : Plot_Vector_Type := Region_Vectors.Empty_Vector;
            begin
               Find_Region (M, A, Coordinate_Type'(Y, X), M (Y, X), Region);
               if Region /= Region_Vectors.Empty_Vector then
                  Regions.Append (Region);
               end if;
            end;
         end if;
      end loop;
   end loop Outer_Loop;

   --  Calculate prices
   for R of Regions loop
      declare
         Perimeter : Natural := Get_Sides (M, R);
      begin
         Price := @ + (Get_Sides (M, R) * Natural (R.Length));
         Put_Line ("Area: " & Natural'Image (Natural (R.Length)) &
                  " Sides: " & Perimeter'Image);
      end;
      exit;
   end loop;

   Put_Line ("Total price: " & Price'Image);
end Day_12_2;
