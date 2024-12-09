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

   function Is_Pair_ordered (Rules  : Rule_Maps.Map;
                             Page   : Page_Number;
                             Before : Page_Number) return Boolean is
   begin
      if Rules.Contains (Page) then
         for I in Rules (Page).Iterate loop
            if Rules (Page) (To_Index (I)) = Rule (Before) then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Is_Pair_ordered;

   function Is_Update_Ordered (Rules  : Rule_Maps.Map;
                               Update : Page_Vectors.Vector) return Boolean is
   begin
      --  Check all possible pairs in the update
      for I in Update.First_Index .. Update.Last_Index loop
         for J in I + 1 .. Update.Last_Index loop
            --  Check if the pair is in order
            if not Is_Pair_ordered (Rules, Update (J), Update (I)) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Is_Update_Ordered;

   procedure Order_Update (Rules  : Rule_Maps.Map;
                           Update : in out Page_Vectors.Vector) is
   begin
      --  Check all possible pairs in the update
      for I in Update.First_Index .. Update.Last_Index loop
         for J in I + 1 .. Update.Last_Index loop
            --  Check if the pair is in order
            if not Is_Pair_ordered (Rules, Update (J), Update (I)) then
               --  Swap the pair
               declare
                  Page : constant Page_Number := Update (J);
               begin
                  Update (J) := Update (I);
                  Update (I) := Page;
               end;
            end if;
         end loop;
      end loop;

   end Order_Update;

   F : File_Type;
   Line_Count : Natural := 0;
   Reading_Rules : Boolean := True;
   Rules : Rule_Maps.Map;
   Sum : Natural := 0;
   Sum_Incorrect : Natural := 0;
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

               if Is_Update_Ordered (Rules, Update) then
                  --  Get middle number and sum
                  declare
                     use type Ada.Containers.Count_Type;
                     Mid_Idx : constant Positive
                       := Integer (Update.Length) / 2 + 1;
                  begin
                     Sum := @ + Natural (Update.Element (Mid_Idx));
                  end;
               else
                  --  Order the incorrectly ordered updates and sum
                  Order_Update (Rules, Update);
                  declare
                     use type Ada.Containers.Count_Type;
                     Mid_Idx : constant Positive
                       := Integer (Update.Length) / 2 + 1;
                  begin
                     Sum_Incorrect := @ + Natural (Update.Element (Mid_Idx));
                  end;
               end if;

            end;
         end if;
      end;
   end loop;

   Close (F);

   Put_Line ("Read updates. Line: " & Line_Count'Image);
   Put_Line ("Total ordered sum: " & Sum'Image);
   Put_Line ("Total incorrectly ordered sum: " & Sum_Incorrect'Image);
end Day_5_1;
