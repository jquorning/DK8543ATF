--
--
--

with Ada.Text_IO;

with Setup;
with Command_Line;
with Database.Jobs;
with Database;
with Parser;
with Web_Server;
with Terminal_IO;
with Interactive;
with Navigate;

procedure To_Do_It is
   Config : Setup.Configuration;
begin
   Command_Line.Parse (Config);

   Interactive.Initialize;
   Web_Server.Startup;
   Database.Open;

   Terminal_IO.Put_Banner;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("List:");
   Navigate.Build_Path (Database.Jobs.Get_Current_Job);
   Terminal_IO.Put_Path;
   Navigate.Refresh_List;
   Terminal_IO.Put_Jobs;

   loop
      Parser.Parse_Input (Interactive.Get_Line);
      exit when Parser.Exit_Program;
   end loop;

   Web_Server.Shutdown;
   Interactive.Finalize;

   Command_Line.Set_Exit_Status (Command_Line.Success);

exception

   when Command_Line.Terminate_Program =>
      null;

end To_Do_It;
