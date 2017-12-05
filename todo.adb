--
--
--

with Database;
with Parser;

procedure Todo is
--   use Database;
begin
   Database.Open;
--   Put_Jobs;
--   Put_Jobs;

--     declare
--        Id : constant Job_Id := Get_Job_Id;
--     begin
--        Create_Job (Id, "Købe julegaver");
--        null;
--     end;

--   Put_Jobs;


   Parser.Put_Banner;
   loop
      Parser.Put_Prompt;
      Parser.Parse_Input (Parser.Get_Input);
      exit when Parser.Exit_Program;
   end loop;

end Todo;
