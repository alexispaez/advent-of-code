pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_2_1 is

   package Natural_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Natural);

   procedure Get_Levels (Line   : String;
                         Report : in out Natural_Vectors.Vector) is
      Finished : Boolean := False;
      Start    : Natural := Line'First;
      Space    : Natural := 0;
   begin
      while Finished = False loop
         Space := Index (Line (Start .. Line'Last), " ");

         if Space /= 0 then
            Report.Append (Natural'Value (
                           Trim (Line (Start .. Space), Both)));
            Start := Space + 1;
         else
            --  Reached end of line
            Report.Append (Natural'Value (
                           Trim (Line (Start .. Line'Last), Both)));
            Finished := True;
         end if;
      end loop;
   end Get_Levels;

   function Is_Safe (Report : Natural_Vectors.Vector) return Boolean is
      type Direction_Type is (Undefined, Ascending, Descending);
      Direction : Direction_Type := Undefined;
   begin
      for I in Report.First_Index .. Report.Last_Index - 1 loop
         case Direction is
            when Undefined =>
               --  Not direction defined, first iteration
               --  Set direction, or exit if no increase found
               if Report.Element (I) < Report.Element (I + 1) then
                  Direction := Ascending;
               elsif Report.Element (I) > Report.Element (I + 1) then
                  Direction := Descending;
               else
                  return False;
               end if;
            when Ascending =>
               --  If the pair is not incrementing, exit with false
               if Report.Element (I) >= Report.Element (I + 1) then
                  return False;
               end if;
            when Descending =>
               --  If the pair is not decrementing, exit with false
               if Report.Element (I) <= Report.Element (I + 1) then
                  return False;
               end if;
         end case;

         --  Check if the increment is between 1 and 3
         declare
            Level_Difference : constant Natural
              := abs (Report.Element (I) - Report.Element (I + 1));
         begin
            if Level_Difference < 1 or else Level_Difference > 3 then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Is_Safe;

   F : File_Type;
   Safe_Reports : Natural := 0;

begin
   --  Open and read the report file
   Open (F, In_File, "input.txt");
   while not End_Of_File (F) loop
      declare
         Report : Natural_Vectors.Vector;
         Line   : constant String := Get_Line (F);
      begin
         --  Get the levels from the line
         Get_Levels (Line, Report);

         --  Check if the report is safe or not
         if Is_Safe (Report) then
            Safe_Reports := @ + 1;
         end if;
      end;
   end loop;
   Close (F);

   Put_Line ("Safe reports found: " & Safe_Reports'Image);
end Day_2_1;
