with Ada.Text_IO;
procedure Puzzle is
   type t_sol is record
      part1 : Integer;
      part2 : Integer;
   end record;

   generic
      with function valid_password (p : Integer) return Boolean;
   package p_runner is
      task type t_runner is
         entry Start (s, e : Integer);
         entry Done (c : in out Integer);
      end t_runner;
   end p_runner;
   package body p_runner is
      task body t_runner is
         s_range, e_range : Integer;
         count : Integer := 0;
      begin
         accept Start (s, e : Integer) do
            s_range := s;
            e_range := e;
         end Start;

         for x in s_range .. e_range loop
            if valid_password (x) then
               count := count + 1;
            end if;
         end loop;

         accept Done (c : in out Integer) do
            c := c + count;
         end Done;

      end t_runner;
   end p_runner;

   function valid_password1 (p : Integer) return Boolean is
      arr : array (1 .. 6) of Integer := (
         p / 100_100,
         (p / 10_000) mod 10,
         (p / 1_000) mod 10,
         (p / 100) mod 10,
         (p / 10) mod 10,
         p mod 10
      );
      seen_double : Boolean := False;
   begin
      for i in arr'Range loop
         if not (i = arr'First) then
            if arr (i) = arr (i - 1) then
               seen_double := True;
            elsif arr (i) < arr (i - 1) then
               return False;
            end if;
         end if;
      end loop;

      return seen_double;
   end valid_password1;

   function valid_password2 (p : Integer) return Boolean is
      arr : array (1 .. 6) of Integer := (
         p / 100_100,
         (p / 10_000) mod 10,
         (p / 1_000) mod 10,
         (p / 100) mod 10,
         (p / 10) mod 10,
         p mod 10
      );
      seen_double : Boolean := False;
      current_seen : Integer := 1;
   begin
      for i in arr'Range loop
         if not (i = arr'First) then
            if arr (i) = arr (i - 1) then
               current_seen := current_seen + 1;
            elsif arr (i) < arr (i - 1) then
               return False;
            else
               if current_seen = 2 then
                  seen_double := True;
               end if;
               current_seen := 1;
            end if;
         end if;
      end loop;
      if current_seen = 2 then
         seen_double := True;
      end if;
      return seen_double;
   end valid_password2;

   sol : t_sol := (0, 0);
begin
   declare
      package p_runner1 is new p_runner (valid_password1);
      package p_runner2 is new p_runner (valid_password2);
      type runner1_arr is array (Integer range <>) of p_runner1.t_runner;
      type runner2_arr is array (Integer range <>) of p_runner2.t_runner;
      runner1 : runner1_arr (1 .. 12);
      runner2 : runner2_arr (1 .. 12);
      range_size : Integer := 769058 - 307237;
      segment_size : Integer := range_size / 12;
   begin
      for I in 0 .. 11 loop
         runner1 (I + 1).Start ((segment_size * I) + 307237, (segment_size * I) + 307237 + segment_size);
         runner2 (I + 1).Start ((segment_size * I) + 307237, (segment_size * I) + 307237 + segment_size);
      end loop;
      for I in 1 .. 12 loop
         runner1 (I).Done (sol.part1);
         runner2 (I).Done (sol.part2);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Puzzle 1 solution:" & sol.part1'Img);
   Ada.Text_IO.Put_Line ("Puzzle 2 solution:" & sol.part2'Img);
end Puzzle;
