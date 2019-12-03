with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
procedure Puzzle is
   
   package ustring_p renames Ada.Strings.Unbounded;
   subtype usting_t is ustring_p.Unbounded_String;
   
   type t_coord is record
      x : Integer;
      y : Integer;
   end record;

   type t_dir is (
      UP,
      DOWN,
      LEFT,
      RIGHT
   );

   type t_instr is record
      dist : Integer;
      dir  : t_dir;
   end record;

   function eq (lhs, rhs : t_coord) return Boolean is
      (lhs.x = rhs.x and lhs.y = rhs.y);

   function lt (lhs, rhs : t_coord) return Boolean is
      (if lhs.x = rhs.x then lhs.y < rhs.y else lhs.x < rhs.x);

   package p_coord_set is new Ada.Containers.Ordered_Sets (t_coord, lt, eq);
   subtype t_coord_set is p_coord_set.Set;   
   package p_coord_map is new Ada.Containers.Ordered_Maps (t_coord, Integer, lt, "=");
   subtype t_coord_map is p_coord_map.Map;
   package p_instr_vec is new Ada.Containers.Vectors (Natural, t_instr, "=");
   subtype t_instr_vec is p_instr_vec.Vector;

   type t_sol is record
      part1 : Integer;
      part2 : Integer;
   end record;

   procedure get_instrs (vec : in out t_instr_vec; s : access String);
   procedure get_instrs (vec : in out t_instr_vec; s : access String) is
      current_string : usting_t := ustring_p.Null_Unbounded_String;
      instr          : t_instr;
      get_dir : Boolean := True;
   begin
      for c of s.all loop
         if get_dir then
            case c is
               when 'D' =>
                  instr.dir := DOWN;
               when 'L' =>
                  instr.dir := LEFT;
               when 'R' =>
                  instr.dir := RIGHT;
               when 'U' =>
                  instr.dir := UP;
               when others =>
                  raise Constraint_Error;
            end case;
            get_dir := False;
         else
            if c = ',' then
               instr.dist := Integer'Value (ustring_p.To_String (current_string));
               vec.Append (instr);
               get_dir := True;
               current_string := ustring_p.Null_Unbounded_String;
            else
               ustring_p.Append (current_string, c);
            end if;
         end if;
      end loop;

      instr.dist := Integer'Value (ustring_p.To_String (current_string));
      vec.Append (instr);
   end get_instrs;

   function do_sol (a, b : t_instr_vec) return t_sol;
   function do_sol (a, b : t_instr_vec) return t_sol is
      coord : t_coord := (0, 0);
      set1 : t_coord_set;
      set2 : t_coord_set;
      map1 : t_coord_map;
      map2 : t_coord_map;
      total_dist : Integer := 0;
   begin
      for instr of a loop
         case instr.dir is
            when UP =>
               for y in coord.y .. coord.y + instr.dist loop
                  if not set1.Contains ((coord.x, y)) then
                     set1.Insert ((coord.x, y));
                     map1.Insert ((coord.x, y), total_dist + abs (y - coord.y));
                  end if;
               end loop;
               coord.y := coord.y + instr.dist;
            when DOWN =>
               for y in coord.y - instr.dist .. coord.y loop
                  if not set1.Contains ((coord.x, y)) then
                     set1.Insert ((coord.x, y));
                     map1.Insert ((coord.x, y), total_dist + abs (y - coord.y));
                  end if;
               end loop;
               coord.y := coord.y - instr.dist;
            when LEFT =>
               for x in coord.x - instr.dist .. coord.x loop
                  if not set1.Contains ((x, coord.y)) then
                     set1.Insert ((x, coord.y));
                     map1.Insert ((x, coord.y), total_dist + abs (x - coord.x));
                  end if;
               end loop;
               coord.x := coord.x - instr.dist;
            when RIGHT =>               
               for x in coord.x .. coord.x + instr.dist loop
                  if not set1.Contains ((x, coord.y)) then
                     set1.Insert ((x, coord.y));
                     map1.Insert ((x, coord.y), total_dist + abs (x - coord.x));
                  end if;
               end loop;
               coord.x := coord.x + instr.dist;
         end case;
         total_dist := total_dist + instr.dist;
      end loop;

      coord := (0, 0);
      total_dist := 0;
      for instr of b loop
         case instr.dir is
            when UP =>
               for y in coord.y .. coord.y + instr.dist loop
                  if not set2.Contains ((coord.x, y)) then
                     set2.Insert ((coord.x, y));
                     map2.Insert ((coord.x, y), total_dist + abs (y - coord.y));
                  end if;
               end loop;
               coord.y := coord.y + instr.dist;
            when DOWN =>
               for y in coord.y - instr.dist .. coord.y loop
                  if not set2.Contains ((coord.x, y)) then
                     set2.Insert ((coord.x, y));
                     map2.Insert ((coord.x, y), total_dist + abs (y - coord.y));
                  end if;
               end loop;
               coord.y := coord.y - instr.dist;
            when LEFT =>
               for x in coord.x - instr.dist .. coord.x loop
                  if not set2.Contains ((x, coord.y)) then
                     set2.Insert ((x, coord.y));
                     map2.Insert ((x, coord.y), total_dist + abs (x - coord.x));
                  end if;
               end loop;
               coord.x := coord.x - instr.dist;
            when RIGHT =>               
               for x in coord.x .. coord.x + instr.dist loop
                  if not set2.Contains ((x, coord.y)) then
                     set2.Insert ((x, coord.y));
                     map2.Insert ((x, coord.y), total_dist + abs (x - coord.x));
                  end if;
               end loop;
               coord.x := coord.x + instr.dist;
         end case;
         total_dist := total_dist + instr.dist;
      end loop;
      declare
         intersect : t_coord_set := set1.Intersection (set2);
         elem : t_coord := (0, 0);
         current_cost : Integer;
         res : Integer := 0;
      begin
         for item of intersect loop
            current_cost := map1.Element (item) + map2.Element (item);
            if elem.x = 0 and elem.y = 0 then
               elem := item;
            elsif ((abs item.x) + (abs item.y)) < ((abs elem.x) + (abs elem.y)) then
               elem := item;
            end if;
            if res = 0 then
               res := current_cost;
            elsif current_cost < res then
               res := current_cost;
            end if;
         end loop;
         Ada.Text_IO.Put_Line ("elem" & elem.x'Img & elem.y'Img);
         return (abs elem.x + abs elem.y, res);
      end;
   end do_sol;

   input : Ada.Text_IO.File_Type;
   sol : t_sol;
   instrs1 : t_instr_vec;
   instrs2 : t_instr_vec;
begin
   Ada.Text_IO.Open (input, Ada.Text_IO.In_File, "input.txt");
   declare
      str1 : aliased String := Ada.Text_IO.Get_Line (input);
      str2 : aliased String := Ada.Text_IO.Get_Line (input);
   begin
      get_instrs (instrs1, str1'Access);
      get_instrs (instrs2, str2'Access);
      sol := do_sol (instrs1, instrs2);
   end;
   Ada.Text_IO.Put_Line ("Puzzle 1 solution:" & sol.part1'Img);
   Ada.Text_IO.Put_Line ("Puzzle 2 solution:" & sol.part2'Img);
end Puzzle;
