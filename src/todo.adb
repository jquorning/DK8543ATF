--
--
--

with Ada.Text_IO;

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

   Database.Get_Jobs (Database.Top_Jobs, Parent => Database.Top_Level);
   Database.Get_Jobs (Database.Cur_Jobs, Parent => Database.Get_Current_Job);

   Terminal_IO.Put_Banner;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Top Jobs:");
   Terminal_IO.Put_Jobs (Database.Top_Jobs);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Current Jobs:");
   Terminal_IO.Put_Jobs (Database.Cur_Jobs);

   loop
      Parser.Parse_Input (Interactive.Get_Line);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;
end Todo;
