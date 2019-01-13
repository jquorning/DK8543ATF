--
--
--

with Ada.Text_IO;

with Database.Jobs;
with Database;
with Parser;
with Web_Server;
with Terminal_IO;
with Interactive;
with Navigate;

procedure What_To_Do is
begin
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
end What_To_Do;
