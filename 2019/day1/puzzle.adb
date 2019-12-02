with Ada.Text_IO;
procedure Puzzle is
   type t_sol is record
      part1 : Integer;
      part2 : Integer;
   end record;

   function do_sol (file : Ada.Text_IO.File_Type) return t_sol;

   function do_sol (file : Ada.Text_IO.File_Type) return t_sol is
      input : Ada.Text_IO.File_Type;
      o : t_sol := (0, 0);
   begin
      while not Ada.Text_IO.End_Of_File (file) loop
         declare
            current : Integer := 0;
            total   : Integer := 0;
         begin
            current := Integer'Value (Ada.Text_IO.Get_Line(file));
            o.part1 := o.part1 + (current / 3 - 2);
            while (current / 3 - 2) > 0 loop
               total := total + current;
               current := current / 3 - 2;
               o.part2 := o.part2 + current;
            end loop;
         end;
      end loop;
      return o;
   end do_sol;

   input : Ada.Text_IO.File_Type;
   sol : t_sol;
begin
   Ada.Text_IO.Open (input, Ada.Text_IO.In_File, "input.txt");
   sol := do_sol (input);
   Ada.Text_IO.Put_Line ("Puzzle 1 solution:" & sol.part1'Img);
   Ada.Text_IO.Put_Line ("Puzzle 2 solution:" & sol.part2'Img);
end Puzzle;
