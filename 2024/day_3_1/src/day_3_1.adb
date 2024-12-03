pragma Ada_2022;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_3_1 is

   function Get_Expression_Numbers (Line  : String;
                                    Idx   : in out Natural;
                                    Left  : out Natural;
                                    Right : out Natural) return Boolean is

      type State_Type is (Looking_For_Expression,
                          Found_Left_Paren,
                          Found_Digit_Left,
                          Found_Comma,
                          Found_Digit_Right);

      State        : State_Type := Looking_For_Expression;
      Left_Number  : Unbounded_String := Null_Unbounded_String;
      Right_Number : Unbounded_String := Null_Unbounded_String;
   begin
      for C of Line (Idx .. Line'Last) loop

         Idx := @ + 1;

         case C is
            when '(' =>
               if State = Looking_For_Expression then
                  --  A left parenthesis while looking for a new mul expression
                  --  is valid
                  State := Found_Left_Paren;
               else
                  --  Invalid expression
                  return False;
               end if;
            when '0' .. '9' =>
               case State is
                  when Found_Left_Paren | Found_Digit_Left =>
                     --  A digit after a left parenthesis or another digit
                     --  is valid
                     State := Found_Digit_Left;
                     Left_Number := @ & C;
                  when Found_Comma | Found_Digit_Right =>
                     --  A digit after a comma or another digit is valid
                     State := Found_Digit_Right;
                     Right_Number := @ & C;
                  when others =>
                     --  Not part of an expression
                     return False;
               end case;
            when ',' =>
               if State = Found_Digit_Left then
                  --  A comma after a digit is valid
                  State := Found_Comma;
               else
                  --  Invalid expression
                  return False;
               end if;
            when ')' =>
               if State = Found_Digit_Right then
                  --  Get number values
                  Left := Integer'Value (To_String (Left_Number));
                  Right := Integer'Value (To_String (Right_Number));
                  --  Completed an expression
                  return True;
               end if;
            when others =>
               --  If any other character is found the expression is invalid
               return False;
         end case;
      end loop;

      return False;
   end Get_Expression_Numbers;

   F            : File_Type;
   Total_Result : Natural := 0;
   Left         : Natural := 0;
   Right        : Natural := 0;

begin
   --  Open and read the program file
   Open (F, In_File, "input.txt");
   while not End_Of_File (F) loop
      declare
         Line         : constant String := Get_Line (F);
         Mul_Position : Natural := 0;
         Idx          : Natural := 1;
      begin
         loop
            Mul_Position := Index (Line (Idx .. Line'Last), "mul");

            exit when Mul_Position = 0;

            --  Found a mul string, try to evaluate its expression
            Idx := Mul_Position + 3;

            if Get_Expression_Numbers (Line, Idx, Left, Right) then
               Total_Result := @ + Left * Right;
            end if;
         end loop;
      end;
   end loop;
   Close (F);

   Put_Line ("Total result of multiplcations: " & Total_Result'Image);
end Day_3_1;
