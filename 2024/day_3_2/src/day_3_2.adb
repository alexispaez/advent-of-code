pragma Ada_2022;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_3_2 is

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
   Mul_Enabled  : Boolean := True;

begin
   --  Open and read the program file
   Open (F, In_File, "../day_3_1/input.txt");
   while not End_Of_File (F) loop
      declare
         type Instruction_Type is (Undefined, Mul_Instr, Do_Instr, Dont_Instr);

         Line           : constant String := Get_Line (F);
         Mul_Position   : Natural := 0;
         Do_Position    : Natural := 0;
         Dont_Position  : Natural := 0;
         First_Position : Natural := 0;
         Instruction    : Instruction_Type := Undefined;
         Idx            : Natural := 1;
      begin
         loop
            --  Get position of next instruction
            First_Position := 0;
            Mul_Position := Index (Line (Idx .. Line'Last), "mul");
            Do_Position := Index (Line (Idx .. Line'Last), "do()");
            Dont_Position := Index (Line (Idx .. Line'Last), "don't()");

            if Mul_Position /= 0 then
               First_Position := Mul_Position;
               Instruction := Mul_Instr;
            end if;

            if Do_Position /= 0 and then Do_Position < First_Position then
               First_Position := Do_Position;
               Instruction := Do_Instr;
            end if;

            if Dont_Position /= 0 and then Dont_Position < First_Position then
               First_Position := Dont_Position;
               Instruction := Dont_Instr;
            end if;

            exit when First_Position = 0; --  No instructions found

            --  Found a mul string, try to evaluate its expression
            case Instruction is
               when Mul_Instr =>
                  Idx := Mul_Position + 3;

                  if Get_Expression_Numbers (Line, Idx, Left, Right) then
                     if Mul_Enabled then
                        Total_Result := @ + Left * Right;
                     end if;
                  end if;
               when Do_Instr =>
                  Mul_Enabled := True;
                  Idx := Do_Position + 4;
               when Dont_Instr =>
                  Mul_Enabled := False;
                  Idx := Dont_Position + 7;
               when others => null;
            end case;
         end loop;
      end;
   end loop;
   Close (F);

   Put_Line ("Total result of multiplications: " & Total_Result'Image);

end Day_3_2;
