--
--
--

with Database;
with Parser;
with Web_Server;

procedure Todo is
begin
   Web_Server.Startup;
   Database.Open;

   Database.Get_Current (Database.Jobs.Current,
                         Database.Lists.Current);

   Database.Get_Lists (Database.Lists);
   Database.Get_Jobs (Database.Jobs);

   Parser.Put_Banner;
   Database.Put_Lists (Database.Lists);
   loop
      Parser.Put_Prompt;
      Parser.Parse_Input (Parser.Get_Input);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
end Todo;
