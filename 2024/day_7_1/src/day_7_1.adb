pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_7_1 is

   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);

   procedure Get_Numbers (Line   : String;
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
   end Get_Numbers;

   --  Define a type for storing operator combinations
   package Operator_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Character);
   use Operator_Vectors;

   --  Define a package to hold all operator combinations
   package Combination_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Operator_Vectors.Vector);

   --  Function to generate all combinations
   function Generate_All_Combinations (S : String; N : Positive)
                                       return Combination_Vectors.Vector is
      Result : Combination_Vectors.Vector :=
                 Combination_Vectors.Empty_Vector;

      --  Helper procedure to recursively generate combinations
      procedure Add_Combinations (Current : Operator_Vectors.Vector;
                                  Depth   : Positive) is
         Next_Current : Operator_Vectors.Vector;
      begin
         if Depth > N then
            --  Add the fully constructed combination to the result
            Result.Append (Current);
         else
            --  Generate branches for each character
            for C of S loop
               Next_Current := Current;
               Next_Current.Append (C);
               Add_Combinations (Next_Current, Depth + 1);
            end loop;
         end if;
      end Add_Combinations;

   begin
      --  Start recursive generation
      Add_Combinations (Operator_Vectors.Empty_Vector, 1);
      return Result;
   end Generate_All_Combinations;

   F : File_Type;
   Total_Calibration : Long_Long_Integer := 0;
   Num_Operators     : Natural := 0;
begin
   --  Open and read the report file
   Open (F, In_File, "input.txt");
   while not End_Of_File (F) loop
      declare
         Line       : constant String := Get_Line (F);
         Start      : Natural := Line'First;
         Test_Value : Long_Long_Integer := 0;
         Numbers    : Natural_Vectors.Vector;
         Combinations : Combination_Vectors.Vector :=
                             Combination_Vectors.Empty_Vector;
         Equation_Proved : Boolean := False;
      begin
         --  Get the equation test value
         Start := Index (Line (Start .. Line'Last), ":");
         if Start /= 0 then
            Test_Value :=
              Long_Long_Integer'Value
                (Trim (Line (Line'First .. Start - 1), Both));
            --  Put (Test_Value'Image);
         end if;
         --  Get the numbers for the equation
         Get_Numbers (Trim (Line (Start + 1 .. Line'Last), Both), Numbers);
         --  Generate the operator combinations fot the equation
         Num_Operators := Numbers.Last_Index;
         --  TODO: Optimise the operator combinations generation
         Combinations := Generate_All_Combinations ("+*", Num_Operators);

         --  Calculate all possible combinations of the equation
         for I in Combinations.First_Index .. Combinations.Last_Index loop
            declare
               Operators : constant Operator_Vectors.Vector :=
                             Combinations.Element (I);
               Num_Index : constant Natural := Numbers.First_Index;
               Op_Index  : Natural := Operators.First_Index;
               Total     : Long_Long_Integer :=
                             Long_Long_Integer (Numbers.Element (Num_Index));
            begin
               for N in Numbers.First_Index .. Numbers.Last_Index - 1 loop
                  if Operators (Op_Index) = '+' then
                     Total := @ + Long_Long_Integer (Numbers.Element (N + 1));
                  else
                     Total := @ * Long_Long_Integer (Numbers.Element (N + 1));
                  end if;
                  Op_Index := @ + 1;
               end loop;

               if Total = Test_Value then
                  Equation_Proved := True;
                  Total_Calibration := @ + Total;
                  --  Exit this loop as equation proven true
                  exit;
               end if;
            end;
         end loop;

         if Equation_Proved = False then
            --  Try with concatenation operator
            Combinations := Generate_All_Combinations ("+*|", Num_Operators);
            --  Calculate all possible combinations of the equation
            for I in Combinations.First_Index .. Combinations.Last_Index loop
               declare
                  Operators : constant Operator_Vectors.Vector :=
                                Combinations.Element (I);
                  Num_Index : constant Natural := Numbers.First_Index;
                  Op_Index  : Natural := Operators.First_Index;
                  Total     : Long_Long_Integer :=
                                Long_Long_Integer
                                  (Numbers.Element (Num_Index));
               begin
                  for N in Numbers.First_Index .. Numbers.Last_Index - 1 loop
                     if Operators (Op_Index) = '+' then
                        Total := @ +
                          Long_Long_Integer (Numbers.Element (N + 1));
                     elsif Operators (Op_Index) = '*' then
                        Total := @ *
                          Long_Long_Integer (Numbers.Element (N + 1));
                     elsif Operators (Op_Index) = '|' then
                        declare
                           Concat : String := Trim (Total'Image, Both) &
                                      Trim
                                        (Numbers.Element (N + 1)'Image, Both);
                        begin
                           Total := Long_Long_Integer'Value (Concat);
                        end;
                     end if;
                     Op_Index := @ + 1;
                  end loop;

                  if Total = Test_Value then
                     Equation_Proved := True;
                     Total_Calibration := @ + Total;
                     --  Exit this loop as equation proven true
                     exit;
                  end if;
               end;
            end loop;
         end if;
      end;
   end loop;

   Close (F);

   Put_Line ("Total calibration result: " & Total_Calibration'Image);
end Day_7_1;
