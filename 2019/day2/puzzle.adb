with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
procedure Puzzle is
   type t_sol is record
      part1 : Integer;
      part2 : Integer;
   end record;

   package p_memory is new Ada.Containers.Vectors (Natural, Integer, "=");
   package ustring_p renames Ada.Strings.Unbounded;
   subtype usting_t is ustring_p.Unbounded_String;
   memory : aliased p_memory.Vector;

   procedure set_memory (m : access p_memory.Vector; f : Ada.Text_IO.File_Type);
   procedure set_memory (m : access p_memory.Vector; f : Ada.Text_IO.File_Type) is
      current_string : usting_t := ustring_p.Null_Unbounded_String;
      val : Integer;
   begin
      while not Ada.Text_IO.End_Of_File (f) loop
         declare
            c : Character;
         begin
            Ada.Text_IO.Get (f, c);
            if c = ',' then
               val := Integer'Value (ustring_p.To_String (current_string));
               m.Append (val);
               current_string := ustring_p.Null_Unbounded_String;
            else
               ustring_p.Append (current_string, c);
            end if;
         end;
      end loop;
      
      val := Integer'Value (ustring_p.To_String (current_string));
      m.Append (val);
   end set_memory;

   function do_part1 (m : in out p_memory.Vector) return Integer is
      idx : Natural := 0;
      c, a, b, o : Integer;
   begin
      c := m.Element (idx);
      while c /= 99 loop
         a := m.Element (m.Element (idx + 1));
         b := m.Element (m.Element (idx + 2));
         o := m.Element (idx + 3); 
         if m.Element (idx) = 1 then
            m.Replace_Element (o, a + b);
         elsif m.Element (idx) = 2 then
            m.Replace_Element (o, a * b);
         else
            Ada.Text_IO.Put_Line (c'Img);
            raise Constraint_Error;
         end if;
         idx := idx + 4;
         c := m.Element (idx);
      end loop;
      return m.Element (0);
   end do_part1;

   function do_part2 (m : p_memory.Vector) return Integer is
      m_do : p_memory.Vector;
      o : Integer;
   begin
      for a in 0 .. 99 loop
         for b in 0 .. 99 loop
            m_do := m;
            m_do.Replace_Element (1, a);
            m_do.Replace_Element (2, b);
            o := do_part1 (m_do);
            if o = 19690720 then
               return 100 * a + b;
            end if;
         end loop;
      end loop;
      return -1;
   end do_part2;

   input : Ada.Text_IO.File_Type;
   sol : t_sol;
begin
   Ada.Text_IO.Open (input, Ada.Text_IO.In_File, "input.txt");
   declare
   begin
      set_memory (memory'Access, input);
      memory.Replace_Element (1, 12);
      memory.Replace_Element (2, 2);
      declare
         m_orig : p_memory.Vector := memory;
      begin
         sol.part1 := do_part1 (memory);
         memory := m_orig;
         sol.part2 := do_part2 (m_orig);
      end;
   end;
   Ada.Text_IO.Put_Line ("Puzzle 1 solution:" & sol.part1'Img);
   Ada.Text_IO.Put_Line ("Puzzle 2 solution:" & sol.part2'Img);
end Puzzle;
