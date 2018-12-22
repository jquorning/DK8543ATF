--
--
--

with Database;
with Parser;
with Web_Server;
with Terminal_IO;
with Interactive;

procedure Todo is
begin
   Interactive.Initialize;
   Web_Server.Startup;
   Database.Open;

   Database.Get_Current (Database.Jobs.Current,
                         Database.Lists.Current);

   Database.Get_Lists (Database.Lists);
   Database.Get_Jobs (Database.Jobs, List => Database.All_Lists);

   Parser.Put_Banner;
   Terminal_IO.Put_Lists (Database.Lists);
   loop
--      Parser.Put_Prompt;
--      Parser.Parse_Input (Parser.Get_Input);
      declare
         Command : constant String := Interactive.Get_Line;
      begin
         Parser.Parse_Input (Command);
      end;
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;
end Todo;
