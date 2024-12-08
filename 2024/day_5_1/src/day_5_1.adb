pragma Ada_2022;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_5_1 is

   type Page_Number is new Natural;

   type Rule is new Page_Number;

   package Page_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Page_Number);
   use Page_Vectors;

   package Rule_Vectors is new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Rule);
   use Rule_Vectors;

   package Rule_Maps is new Ada.Containers.Ordered_Maps
       (Key_Type        => Page_Number,
        Element_Type    => Rule_Vectors.Vector);
   use Rule_Maps;

   F : File_Type;
   Line_Count : Natural := 0;
   Reading_Rules : Boolean := True;
   Rules : Rule_Maps.Map;
begin
   Open (F, In_File, "input.txt");

   while not End_Of_File (F) loop
      declare
         Line : constant String := Get_Line (F);
      begin
         Line_Count := @ + 1;

         if Reading_Rules then
            if Line = "" then
               Put_Line ("Read rules. Line: " & Line_Count'Image);
               Reading_Rules := False;
            else
               --  Read rules
               declare
                  Before : constant Page_Number
                    := Page_Number'Value (Line (1 .. 2));
                  R      : constant Rule
                    := Rule'Value (Line (4 .. 5));
               begin
                  if Rules.Contains (Before) then
                     Rules (Before).Append (R);
                  else
                     Rules.Include (Before, Rule_Vectors.Empty_Vector);
                     Rules (Before).Append (R);
                  end if;
               end;
            end if;
         else
            declare
               Finished : Boolean := False;
               Start    : Natural := Line'First;
               Comma    : Natural := 0;
               Update   : Page_Vectors.Vector;
            begin
               --  Read and process updates
               while Finished = False loop
                  Comma := Index (Line (Start .. Line'Last), ",");

                  if Comma /= 0 then
                     Update.Append (Page_Number'Value (
                                    Trim (Line (Start .. Comma - 1), Both)));
                     Start := Comma + 1;
                  else
                     --  Reached end of line
                     Update.Append (Page_Number'Value (
                                    Trim (Line (Start .. Line'Last), Both)));
                     Finished := True;
                  end if;
               end loop;

               for I in Update.First_Index .. Update.Last_Index loop
                  for J in I + 1 .. Update.Last_Index loop
                     Put ("(" & Page_Number'Image (Update (I)));
                     Put (Page_Number'Image (Update (J)) & ")");
                  end loop;
                  New_Line;
               end loop;
               New_Line;

               exit;
               --  --  Print update to check
               --  for N of Update loop
               --     Put (N'Image);
               --  end loop;
               --  New_Line;
            end;
         end if;
      end;
   end loop;

   --  for R in Rules.Iterate loop
   --     Put (Key (R)'Image & ": ");
   --     for I in Rules (R).Iterate loop
   --        Put (Rule'Image (Rules (R)(To_Index(I))));
   --     end loop;
   --     New_Line;
   --  end loop;
   Put_Line ("Read updates. Line: " & Line_Count'Image);

   Close (F);
end Day_5_1;
