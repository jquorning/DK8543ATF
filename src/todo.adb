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

   Terminal_IO.Put_Banner;
   Terminal_IO.Put_Lists (Database.Lists);

   loop
      Parser.Parse_Input (Interactive.Get_Line);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;
end Todo;
