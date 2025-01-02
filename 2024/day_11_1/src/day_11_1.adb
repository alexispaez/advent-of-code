pragma Ada_2022;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day_11_1 is

   type Pebble is new Long_Long_Integer;

   package Pebble_Vectors is
     new Ada.Containers.Vectors (Natural, Pebble);

   procedure Put (Pebble_Line : Pebble_Vectors.Vector) is
   begin
      for P of Pebble_Line loop
         Put (P'Image);
      end loop;
      New_Line;
   end Put;

   function Blink (Pebble_Line : Pebble_Vectors.Vector)
                   return Pebble_Vectors.Vector is
      New_Pebble_Line : Pebble_Vectors.Vector := Pebble_Vectors.Empty_Vector;
   begin
      for P of Pebble_Line loop
         if P = 0 then
            New_Pebble_Line.Append (1);
         else
            declare
               S : constant String := Trim (P'Image, Both);
               L : constant Natural := S'Length;
            begin
               --  Even number of digits
               if L mod 2 = 0 then
                  --  Put (S (1 .. L / 2) & " " & S (L / 2 + 1 .. S'Last));
                  New_Pebble_Line.Append (Pebble'Value (S (1 .. L / 2)));
                  New_Pebble_Line.Append
                    (Pebble'Value (S (L / 2 + 1 .. S'Last)));
               else
                  --  Any other case
                  New_Pebble_Line.Append (P * 2024);
               end if;
            end;
         end if;
      end loop;

      return New_Pebble_Line;
   end Blink;

   Pebble_Line : Pebble_Vectors.Vector :=
     [8435,
      234,
      928434,
      14,
      0,
      7,
      92446,
      8992692];
begin

   Put (Pebble_Line);

   for I in 1 .. 25 loop
      Pebble_Line := Blink (Pebble_Line);
   end loop;

   Put_Line ("I have:" & Pebble_Line.Length'Image & " stones.");
end Day_11_1;
